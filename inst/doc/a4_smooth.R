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

## -----------------------------------------------------------------------------
set.seed(1)
s = sim_dfa(num_trends = 1, num_years = 1000, num_ts = 4,
            loadings_matrix = matrix(nrow = 4, ncol = 1, rnorm(4 * 1,
    1, 0.1)), sigma=0.05)

## -----------------------------------------------------------------------------
matplot(t(s$y_sim), type="l")

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(1)
#  fit = fit_dfa(y = s$y_sim, num_trends = 1,
#                trend_model = "bs", n_knots = 7)

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(1)
#  fit = fit_dfa(y = s$y_sim, num_trends = 1,
#                trend_model = "bs", n_knots = 14)

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(1)
#  fit = fit_dfa(y = s$y_sim, num_trends = 1,
#                trend_model = "ps", n_knots = 7)

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(1)
#  fit = fit_dfa(y = s$y_sim, num_trends = 1,
#                trend_model = "gp", n_knots = 7)

