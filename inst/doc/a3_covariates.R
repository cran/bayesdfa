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

## ----simulate-data-obs--------------------------------------------------------
set.seed(1)
sim_dat <- sim_dfa(
  num_trends = 1,
  num_years = 20,
  num_ts = 4
)

## -----------------------------------------------------------------------------
cov = expand.grid("time"=1:20, "timeseries"=1:4, "covariate"=1)
cov$value = rnorm(nrow(cov),0,0.1)

for(i in 1:nrow(cov)) {
  sim_dat$y[cov$timeseries[i],cov$time[i]] = sim_dat$pred[cov$timeseries[i],cov$time[i]] +
  c(0.1,0.2,0.3,0.4)[cov$timeseries[i]]*cov$value[i]
}

## ----results='hide', warning=FALSE, message=FALSE-----------------------------
mod = fit_dfa(y = sim_dat$y, obs_covar = cov, num_trends = 1,
  chains=chains, iter=iter)

## -----------------------------------------------------------------------------
plot(c(sim_dat$x), xlab="Time", ylab="True trend")

## -----------------------------------------------------------------------------
plot_trends(rotate_trends(mod)) + ylab("Estimated trend") + theme_bw()

## -----------------------------------------------------------------------------
cov = cov[which(cov$timeseries!=4),]

## ----eval=FALSE, results='hide', warning=FALSE, message=FALSE-----------------
# mod = fit_dfa(y = sim_dat$y, obs_covar = cov, num_trends = trends,
#   chains=chains)

## ----simulate-data------------------------------------------------------------
set.seed(1)
sim_dat <- sim_dfa(
  num_trends = 2,
  num_years = 20,
  num_ts = 3
)

## -----------------------------------------------------------------------------
cov = rnorm(20, 0, 1)
b_pro = c(1,0.3)
x = matrix(0,2,20)

for(i in 1:2) {
  x[i,1] = cov[1]*b_pro[i]
}

for(i in 2:length(cov)) {
  x[1,i] = x[1,i-1] + cov[i]*b_pro[1] + rnorm(1,0,1)
  x[2,i] = x[2,i-1] + cov[i]*b_pro[2] + rnorm(1,0,1)
}

y = sim_dat$Z %*% x

## ----eval=FALSE, results='hide', warning=FALSE, message=FALSE-----------------
# pro_cov = expand.grid("trend"=1:2, "time"=1:20, "covariate"=1)
# pro_cov$value = cov[pro_cov$time]
# 
# mod = fit_dfa(y = sim_dat$y, pro_covar = pro_cov, num_trends = 2,
#   chains=chains, iter=iter)
# 

