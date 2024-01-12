## ----set-knitr-options, cache=FALSE, echo=FALSE, warning=FALSE, message=FALSE----
library("knitr")
opts_chunk$set(message = FALSE, fig.width = 5.5)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(bayesdfa)
library(ggplot2)
library(dplyr)
library(rstan)
chains = 1
iter = 10

## ----simulate-data------------------------------------------------------------
set.seed(1)
sim_dat <- sim_dfa(
  num_trends = 2,
  num_years = 20,
  num_ts = 4
)

## ----simulate-data-plot, echo=FALSE, fig.align='center', fig.cap="Simulated data, from a model with 2 latent trends and no extremes.\\label{fig:simulate-data-plot}"----
df = data.frame("time" = rep(1:20,4),"y"=c(t(sim_dat$y_sim)), 
                "ts"=as.factor(sort(rep(1:4,20))))
ggplot(df, aes(time,y,group=ts,col=ts)) + geom_line() + theme_bw() + 
  xlab("Time") + ylab("Observed data")

## ----fit-1-trend, message=FALSE, warning=FALSE, results='hide'----------------
f1 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 1, scale="zscore",
  iter = iter, chains = chains, thin = 1
)

## -----------------------------------------------------------------------------
is_converged(f1, threshold = 1.05)

## ----rot-1-trend, warning=FALSE, message=FALSE, results='hide'----------------
r <- rotate_trends(f1)

## -----------------------------------------------------------------------------
names(r)

## ----plot-1-trend, fig.align='center', fig.cap="Estimated trend and 95% CI for a 1-trend DFA model applied to simulated data.\\label{fig:simulate-data-plot}"----
plot_trends(r) + theme_bw()

## ----plot-1-fitted-example, fig.align='center', fig.cap="Model predicted values from the 1-trend DFA model applied to simulated data.\\label{fig:fitted-example}"----
plot_fitted(f1) + theme_bw()

## ----fit-models, warning=FALSE, results='hide', message=FALSE-----------------
f2 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 2, scale="zscore",
  iter = iter, chains = chains, thin = 1
)
r2 <- rotate_trends(f2)

f3 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 3, scale="zscore",
  iter = chains, chains = chains, thin = 1
)
r3 <- rotate_trends(f3)

## ----plot-2-fitted-example, fig.align='center', fig.cap="Model predicted values from the 2-trend DFA model applied to simulated data.\\label{fig:fitted-example2}"----
plot_fitted(f2) + theme_bw()

## -----------------------------------------------------------------------------
round(r2$Z_rot_mean, 2)

## ----plot-loadings, fig.align='center', fig.cap="Estimated loadings from the 2-trend DFA model.\\label{fig:plot-loadings}"----
plot_loadings(r2) + theme_bw()

## ----loo, warning=FALSE, message=FALSE----------------------------------------
loo1 <- loo(f1)
loo1$estimates

## ----eval=FALSE---------------------------------------------------------------
#  m <- find_dfa_trends(
#    y = s$y_sim, iter = iter,
#    kmin = 1, kmax = 5, chains = chains, compare_normal = TRUE,
#    variance = c("equal", "unequal")
#  )

## ----simulate-data2-----------------------------------------------------------
set.seed(1)
sim_dat <- sim_dfa(
  num_trends = 2,
  num_years = 20, num_ts = 4,
  extreme_value = 6
)

## ----simulate-data-plot2, fig.align='center', fig.cap="Simulated data, from a model with 2 latent trends and an extreme in the midpoint of the time series.\\label{fig:simulate-data-plot2}"----
matplot(t(sim_dat$y_sim), type = "l", ylab = "Response", xlab = "Time")

## ----simulate-data-plot3, fig.align='center', fig.cap="Simulated data (z-scored), from a model with 2 latent trends and an extreme in the midpoint of the time series.\\label{fig:simulate-data-plot3}"----
matplot(scale(t(sim_dat$y_sim)), type = "l", ylab = "Response", xlab = "Time")

## ----fit-2-trend-extreme, message=FALSE, warning=FALSE, results='hide'--------
t2 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 2, scale="zscore",
  iter = iter, chains = chains, thin = 1, estimate_nu = TRUE
)

## ----fit-extreme-dfa, fig.align='center', fig.cap="Estimated trends, from a model with 2 latent trends Student-t deviations.\\label{fig:fit-extreme-dfa}"----
r <- rotate_trends(t2)
plot_trends(r) + theme_bw()

## ----plot-extreme-loadings, fig.align='center', fig.cap="Estimated loadings, from a model with 2 latent trends Student-t deviations.\\label{fig:plot-extreme-loadings}"----
plot_loadings(r) + theme_bw()

## ----eval=FALSE---------------------------------------------------------------
#  find_swans(r, plot = FALSE, threshold = 1 / 1000)

## ----summarize-nu-------------------------------------------------------------
summary(rstan::extract(t2$model, "nu")[[1]])

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  f <- fit_dfa(..., family = "poisson")

## ----echo=FALSE, results='asis'-----------------------------------------------
m = cbind(c("gaussian","lognormal","gamma","binomial","poisson","nbinom2"), 
                 c("identity","log","log","logit","log","log"))
colnames(m) = c("Family","link")
knitr::kable(m)

## ----echo=FALSE, results='asis'-----------------------------------------------
Z = matrix("",5,3)
for(i in 1:5) {
  for(j in 1:3) {
    Z[i,j] = paste0("z[",i,",",j,"]")
  }
}
Z[1,2:3] = "0"
Z[2,3] = "0"

colnames(Z) = c("Trend 1","Trend 2","Trend 3")
knitr::kable(Z)

## ----echo=FALSE, results='asis'-----------------------------------------------
Z = matrix("",5,3)
for(i in 1:5) {
  for(j in 1:3) {
    Z[i,j] = paste0("z[",i,",",j,"]")
  }
}
colnames(Z) = c("Trend 1", "Trend 2", "Trend 3")
knitr::kable(Z)

## ----eval = FALSE-------------------------------------------------------------
#  fit <- fit_dfa(..., estimate_trend_ar = TRUE)

## ----eval = FALSE, warning=FALSE, message=FALSE, results='hide'---------------
#  reg_mod <- fit_regimes(
#    y = r$trends_mean[1, ],
#    sds = (r$trends_upper - r$trends_mean)[1, ] / 1.96,
#    n_regimes = 2,
#    iter = 50, chains = 1
#  )

## ----plot-regime, eval=FALSE, fig.align='center', fig.cap="Estimated regimes, from a HMM model applied to the first trend of a 2-trend DFA model with Student-t deviations.\\label{fig:plot-regime}"----
#  plot_regime_model(reg_mod)

## ----plot-regime-flipped, eval=FALSE, fig.align='center', fig.cap="Estimated regimes (after flipping), from a HMM model applied to the first trend of a 2-trend DFA model with Student-t deviations.\\label{fig:plot-regime-flipped}"----
#  plot_regime_model(reg_mod, flip_regimes = TRUE)

## -----------------------------------------------------------------------------
set.seed(1)
sim_dat <- sim_dfa(
  num_trends = 2,
  num_years = 20,
  num_ts = 4
)

df <- data.frame(obs = c(sim_dat$y_sim), time = sort(rep(1:20,4)),
                 ts = rep(1:4,20))
df$se <- runif(nrow(df), 0.6, 0.8)
df$se[which(df$ts == 2)] = 0.2

## -----------------------------------------------------------------------------
df$weights <- (1 / df$se)^2

## -----------------------------------------------------------------------------
f2 <- fit_dfa(
  y = df, num_trends = 2, scale="zscore",
  iter = 500, chains = 1, thin = 1,
  inv_var_weights = "weights", data_shape = "long"
)

## ----eval=FALSE---------------------------------------------------------------
#  f2 <- fit_dfa(
#    y = df, num_trends = 2, scale="zscore",
#    iter = 500, chains = 1, thin = 1,
#    likelihood_weights = "weights", data_shape = "long"
#  )

