#' Simulate from a DFA
#'
#' @param num_trends The number of trends.
#' @param num_years The number of years.
#' @param num_ts The number of timeseries.
#' @param loadings_matrix A loadings matrix. The number of rows should match the
#'   number of timeseries and the number of columns should match the number of
#'   trends. Note that this loadings matrix will be internally manipulated by
#'   setting some elements to 0 and constraining some elements to 1 so that the
#'   model can be fitted. See [fit_dfa()]. See the outfit element `Z` in
#'   the returned list is to see the manipulated loadings matrix. If not
#'   specified, a random matrix `~ N(0, 1)` is used.
#' @param sigma A vector of standard deviations on the observation error. Should
#'   be of the same length as the number of trends. If not specified, random
#'   numbers are used `rlnorm(1, meanlog = log(0.2), 0.1)`.
#' @param varIndx Indices of unique observation variances. Defaults to `c(1, 1,
#'   1, 1)`. Unique observation error variances would be specified as `c(1, 2, 3,
#'   4)` in the case of 4 time series.
#' @param trend_model The type of trend model. Random walk (`"rw"`) or basis
#'   spline (`"bs"`)
#' @param spline_weights A matrix of basis function weights that is used
#'   if `trend_model = "bs"`. The number of columns should correspond to
#'   the number of knots and the number of rows should correspond to the
#'   number of trends.
#' @param extreme_value Value added to the random walk in the extreme time step.
#'   Defaults to not included.
#' @param extreme_loc Location of single extreme event in the process. The same
#'   for all processes, and defaults to `round(n_t/2)` where `n_t` is the time
#'   series length
#' @param nu_fixed Nu is the degrees of freedom parameter for the
#'   t-distribution, defaults to 100, which is effectively normal.
#' @param user_supplied_deviations An optional matrix of deviations for the trend
#'   random walks. Columns are for trends and rows are for each time step.
#' @export
#' @return A list with the following elements: `y_sim` is the simulated data,
#'   pred is the true underlying data without observation error added, `x` is
#'   the underlying trends, `Z` is the manipulated loadings matrix that is fed
#'   to the model.
#' @importFrom stats rlnorm rnorm rt
#' @importFrom splines splineDesign
#' @examples
#' x <- sim_dfa(num_trends = 2)
#' names(x)
#' matplot(t(x$y_sim), type = "l")
#' matplot(t(x$x), type = "l")
#'
#' set.seed(42)
#' x <- sim_dfa(extreme_value = -4, extreme_loc = 10)
#' matplot(t(x$x), type = "l")
#' abline(v = 10)
#' matplot(t(x$pred), type = "l")
#' abline(v = 10)
#'
#' set.seed(42)
#' x <- sim_dfa()
#' matplot(t(x$x), type = "l")
#' abline(v = 10)
#' matplot(t(x$pred), type = "l")
#' abline(v = 10)
#' @export

sim_dfa <- function(num_trends = 1,
                    num_years = 20,
                    num_ts = 4,
                    loadings_matrix = matrix(
                      nrow = num_ts, ncol = num_trends,
                      rnorm(num_ts * num_trends, 0, 1)
                    ),
                    sigma = rlnorm(1, meanlog = log(0.2), 0.1),
                    varIndx = rep(1, num_ts),
                    trend_model = c("rw", "bs"),
                    spline_weights = matrix(ncol = 6, nrow = num_trends,
                      data = rnorm(6 * num_trends)),
                    extreme_value = NULL,
                    extreme_loc = NULL,
                    nu_fixed = 100,
                    user_supplied_deviations = NULL) {
  y_ignore <- matrix(rnorm(num_ts * num_years), nrow = num_ts, ncol = num_years)

  trend_model <- match.arg(trend_model)
  d <- fit_dfa(y_ignore,
    num_trends = num_trends, estimation = "none", scale = "center",
    varIndx = varIndx, nu_fixed = nu_fixed, trend_model = "rw"
  )

  Z <- loadings_matrix
  y <- vector(mode = "numeric", length = d$sampling_args$data$N)

  for (k in seq_len(d$sampling_args$data$K)) {
    Z[k, k] <- abs(Z[k, k]) # add constraint for Z diagonal
  }
  # fill in 0s
  for (k in seq_len(d$sampling_args$data$K)) {
    for (p in seq_len(d$sampling_args$data$P)) {
      if (p < k) Z[p, k] <- 0
    }
  }

  x <- matrix(nrow = d$sampling_args$data$K, ncol = d$sampling_args$data$N) # random walk-trends

  if (trend_model == "rw") {
    # initial state for each trend
    for (k in seq_len(d$sampling_args$data$K)) {
      if (!is.null(user_supplied_deviations)) {
        devs <- user_supplied_deviations[, k]
      } else {
        devs <- rt(d$sampling_args$data$N, df = d$sampling_args$data$nu_fixed)
      }

      x[k, 1] <- rnorm(1, 0, 1)
      if (is.null(extreme_value)) {
        for (t in seq(2, d$sampling_args$data$N)) {
          x[k, t] <- x[k, t - 1] + devs[t] # random walk
        }
      } else {
        if (is.null(extreme_loc)) extreme_loc <- round(num_years / 2)
        for (t in 2:(extreme_loc - 1)) {
          x[k, t] <- x[k, t - 1] + devs[t] # random walk
        }
        # only include extreme in first trend
        if (k == 1) {
          x[1, extreme_loc] <- x[1, extreme_loc - 1] + extreme_value
        } else {
          x[k, extreme_loc] <- x[k, extreme_loc - 1] + devs[t]
        }
        for (t in seq(extreme_loc + 1, d$sampling_args$data$N)) {
          x[k, t] <- x[k, t - 1] + devs[t] # random walk
        }
      }
    }
  } else if (trend_model == "bs") {
    # num_years <- 25
    # spline_weights <- matrix(ncol = 7, nrow = 3, data = rnorm(21))
    df <- ncol(spline_weights)
    degree <- 3
    intercept <- 1L
    # adapted from splines::bs
    ord <- 1 + degree
    Boundary.knots <- c(1, num_years)
    nIknots <- df - ord + (1L - intercept)
    knots <- seq(0, 1, length.out = nIknots + 2L)[-c(1L, nIknots + 2L)]
    knots <- quantile(seq(1, num_years), knots)
    Aknots <- sort(c(rep(Boundary.knots, ord), knots))
    X_spline <- t(splineDesign(Aknots, x = seq(1, num_years), ord))
    x <- spline_weights %*% X_spline
    # matplot(t(x), type = "l", lty = 1)
  } else {
    stop("Trend model not defined", call. = FALSE)
  }

  pred <- Z %*% x
  for (i in seq_len(d$sampling_args$data$n_pos)) {
    y[i] <- rnorm(
      1, pred[d$sampling_args$data$row_indx_pos[i], d$sampling_args$data$col_indx_pos[i]],
      sigma[d$sampling_args$data$varIndx[d$sampling_args$data$row_indx_pos[i]]]
    )
  }
  y_sim <- matrix(y, nrow = d$sampling_args$data$P)
  list(y_sim = y_sim, pred = pred, x = x, Z = Z, sigma = sigma)
}
