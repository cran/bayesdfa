## ----set-knitr-options, cache=FALSE, echo=FALSE, warning=FALSE, message=FALSE----
library("knitr")
opts_chunk$set(message = FALSE, fig.width = 5.5)

## ---- message=FALSE, warning=FALSE--------------------------------------------
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

## -----------------------------------------------------------------------------
set.seed(1)
sim_dat$x[1,] = cumsum(rnorm(n=ncol(sim_dat$x), 0, 0.1))
sim_dat$x[2,] = cumsum(rnorm(n=ncol(sim_dat$x), 0, 1))

## ----simulate-data-plot, fig.align='center', fig.cap="Simulated data, from a model with 2 latent trends and no extremes.\\label{fig:simulate-data-plot}"----
matplot(t(sim_dat$x),
  type = "l",
  ylab = "Response", xlab = "Time"
)

## -----------------------------------------------------------------------------
sim_dat$pred = sim_dat$Z %*% sim_dat$x
for(i in 1:nrow(sim_dat$pred)) {
  for(j in 1:ncol(sim_dat$pred)) {
    sim_dat$y_sim[i,j] = sim_dat$pred[i,j] + rnorm(1,0,0.1)
  }
}

## ----fit-model, message=FALSE, warning=FALSE, results='hide'------------------
f1 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 2, scale="zscore",
  iter = iter, chains = chains, thin = 1
)
r1 <- rotate_trends(f1)

## ----fit-model-2, message=FALSE, warning=FALSE, results='hide'----------------
f2 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 2, scale="zscore", estimate_process_sigma = TRUE,
  equal_process_sigma = FALSE,
  iter = iter, chains = chains, thin = 1
)
r2 <- rotate_trends(f2)

## ----fit-model-3, message=FALSE, warning=FALSE, results='hide'----------------
f3 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 2, scale="center", estimate_process_sigma = TRUE,
  equal_process_sigma = FALSE,
  iter = iter, chains = chains, thin = 1
)
r3 <- rotate_trends(f3)

## ----fit-model-4, message=FALSE, warning=FALSE, results='hide'----------------
f4 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 2, scale="zscore", estimate_process_sigma = TRUE,
  equal_process_sigma = TRUE,
  iter = iter, chains = chains, thin = 1
)
r4 <- rotate_trends(f4)

f5 <- fit_dfa(
  y = sim_dat$y_sim, num_trends = 2, scale="center", estimate_process_sigma = TRUE,
  equal_process_sigma = TRUE,
  iter = iter, chains = chains, thin = 1
)
r5 <- rotate_trends(f5)

## -----------------------------------------------------------------------------
print(round(sim_dat$Z,2))

## -----------------------------------------------------------------------------
print(round(r1$Z_rot_mean,2))

## -----------------------------------------------------------------------------
print(round(r2$Z_rot_mean,2))

## -----------------------------------------------------------------------------
print(round(r3$Z_rot_mean,2))

## -----------------------------------------------------------------------------
print(round(r4$Z_rot_mean,2))

## -----------------------------------------------------------------------------
print(round(r5$Z_rot_mean,2))

## ----echo=FALSE---------------------------------------------------------------
m = matrix(0,5,2)
m[,1] = 1:5
m[1,2] = sum((r1$Z_rot_mean - sim_dat$Z)^2)
m[2,2] = sum((r2$Z_rot_mean - sim_dat$Z)^2)
m[3,2] = sum((r3$Z_rot_mean - sim_dat$Z)^2)
m[4,2] = sum((r4$Z_rot_mean - sim_dat$Z)^2)
m[5,2] = sum((r5$Z_rot_mean - sim_dat$Z)^2)
colnames(m) = c("Model", "RMSE-loadings")
knitr::kable(m)

## ----echo=FALSE---------------------------------------------------------------
m = matrix(0,5,2)
m[,1] = 1:5
m[1,2] = sum((r1$trends_mean - sim_dat$x)^2)
m[2,2] = sum((r2$trends_mean - sim_dat$x)^2)
m[3,2] = sum((r3$trends_mean - sim_dat$x)^2)
m[4,2] = sum((r4$trends_mean - sim_dat$x)^2)
m[5,2] = sum((r5$trends_mean - sim_dat$x)^2)
colnames(m) = c("Model", "RMSE-trends")
knitr::kable(m)

