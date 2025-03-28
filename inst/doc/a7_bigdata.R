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
# fit <- fit_dfa(..., estimation = "sampling")

## -----------------------------------------------------------------------------
set.seed(123)
m <- fit_dfa(y = s$y_sim, estimation = "optimizing")

## -----------------------------------------------------------------------------
names(m$model)

## -----------------------------------------------------------------------------
m$model$return_code

## -----------------------------------------------------------------------------
set.seed(124)
m <- fit_dfa(y = s$y_sim, estimation = "optimizing")

## -----------------------------------------------------------------------------
m$model$return_code

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
# m <- fit_dfa(y = s$y_sim, estimation = "vb", seed=123)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
# m <- fit_dfa(y = s$y_sim, estimation = "vb", seed=123, iter=20000,
#              tol_rel_obj = 0.005, output_samples = 2000)

