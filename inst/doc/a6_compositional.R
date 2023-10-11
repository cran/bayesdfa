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
s = sim_dfa(num_trends = 2, num_years = 20,
  num_ts = 5)

## -----------------------------------------------------------------------------
m = matrix(0, nrow=5,ncol=2)
m[1,] = c(0.8, 0.2) # time series # 1 is 80% trend 1
m[2,] = c(0.9, 0.1) # time series # 2 is 90% trend 1
m[3,] = c(0.3, 0.7) # time series # 3 is 30% trend 1
m[4,] = c(0.35, 0.65) # time series # 4 is 35% trend 1
m[5,] = c(0.7, 0.2) # time series # 5 is 70% trend 1

## -----------------------------------------------------------------------------
pred = m%*%s$x
y = pred + matrix(rnorm(nrow(pred)*ncol(pred),0,0.1), nrow=nrow(pred), ncol = ncol(pred))

## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
fit <- fit_dfa(y = y, iter = iter, chains = chains, num_trends = 2, seed = 42,
    z_model = "proportion",scale="center")

## -----------------------------------------------------------------------------
pars = rstan::extract(fit$model,permuted=TRUE)
rounded_Z = round(apply(pars$Z,c(2,3),mean),2)
print(rounded_Z[,c(2,1)])

## -----------------------------------------------------------------------------
x = apply(pars$x, c(2,3), mean)[c(2,1),]
matplot(t(rbind(x,s$x)))

## -----------------------------------------------------------------------------
set.seed(1)
s = sim_dfa(num_trends = 3, num_years = 20,
  num_ts = 5)

## -----------------------------------------------------------------------------
m = matrix(0, nrow=5,ncol=3)
m[1,] = c(0.31, 0.48,0.21) # time series # 1
m[2,] = c(0.25, 0.04, 0.71) # time series # 2
m[3,] = c(0.21, 0.28, 0.51) # time series # 3
m[4,] = c(0.6, 0.02, 0.38) # time series # 4
m[5,] = c(0.15, 0.21, 0.64) # time series # 5

## -----------------------------------------------------------------------------
pred = m%*%s$x
y = pred + matrix(rnorm(nrow(pred)*ncol(pred),0,0.01), nrow=nrow(pred), ncol = ncol(pred))

## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
fit <- fit_dfa(y = y, iter = iter, chains = chains, num_trends = 3, seed = 42,
    z_model = "proportion",scale="center")

## ----echo=FALSE---------------------------------------------------------------
pars = rstan::extract(fit$model,permuted=TRUE)
rounded_Z = round(apply(pars$Z,c(2,3),mean),2)

df = data.frame("value"=c(rounded_Z), "id" = "estimated",
  "trend"=as.factor(sort(rep(1:3,5))), "ts" = as.factor(rep(1:5,3)))

df2 = data.frame("value"=c(m[,c(3,2,1)]), "id" = "true",
  "trend"=as.factor(sort(rep(1:3,5))), "ts" = as.factor(rep(1:5,3)))

ggplot(data=rbind(df,df2), aes(ts,value,group=trend,col=trend,
  fill=trend,shape=id)) + 
  geom_point(size=4) +
  xlab("Time series") + ylab("Value")

