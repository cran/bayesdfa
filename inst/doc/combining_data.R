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
#library(viridis)

## -----------------------------------------------------------------------------
set.seed(123)
loadings = matrix(0, 3, 1)
loadings[1,1] = c(0.1)
loadings[2:3,1] = runif(2, 0.4,1)
round(loadings,3)
sim = sim_dfa(num_trends = 1, num_years = 100, 
  num_ts = 3, loadings_matrix = loadings, 
  sigma=0.6)

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
#id variable for position in matrix 
Y = as.data.frame(sim$y_sim)
#as.data.frame(t(scale(t(Y))))
Y$ts <- as.factor(1:nrow(Y))
plot_data <- reshape2::melt(Y,id.var="ts")
plot_data$x = as.numeric(substr(plot_data$variable, 2, length(plot_data$variable)))
g1 = ggplot(plot_data, aes(x=x,y=value,group=ts,colour=ts)) +
  geom_point()+
  geom_line() + xlab("Time") +
  theme_bw()#+ scale_color_viridis(end=0.8, discrete = TRUE)


g1
#grid.arrange(g1,g2,nrow=2)

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
Y = as.data.frame(t(scale(t(sim$y_sim))))
Y$ts <- as.factor(1:nrow(Y))
plot_data <- reshape2::melt(Y,id.var="ts")
plot_data$x = as.numeric(substr(plot_data$variable, 2, length(plot_data$variable)))
g2 = ggplot(plot_data, aes(x=x,y=value,group=ts,colour=ts)) +
  geom_point()+
  geom_line() + xlab("Time") + 
  ylab("Standardized time series") +
  theme_bw() #+ 
  #scale_color_viridis(end=0.8, discrete = TRUE)
g2

## ----results='hide'-----------------------------------------------------------
fit_1 = fit_dfa(y = sim$y_sim[,51:100], num_trends = 1, chains=chains, iter=iter)
r = rotate_trends(fit_1)

## -----------------------------------------------------------------------------
round(r$Z_rot_mean,3)

## ----results='hide', warning=FALSE, message=FALSE-----------------------------

output = expand.grid("ts_start"=c(0,25,50), 
  "x"=1:100, "estimated_trend"=NA, "obs"=NA)

l = matrix(0, 3, 3)

for(i in 1:nrow(l)) {
  idx = c(1,26,51) # seq(1,60,10)[nrow(l)+1-i]
  Y = sim$y_sim
  Y = t(scale(t(Y)))
  Y[1,1:(idx-1)] = NA
  Y[2:3,1:50] = NA
  fit_2 = fit_dfa(y = Y, num_trends = 1, chains=1, iter=10, zscore = FALSE)
  r = rotate_trends(fit_2)
  l[i,] = c(r$Z_rot_mean)
  output$estimated_trend[which(output$ts_start==(idx-1))] = scale((r$Z_rot_mean %*% r$trends_mean)[2,])
  output$obs[which(output$ts_start==(idx-1))] = Y[2,51:100]
}

## ----echo=FALSE---------------------------------------------------------------
#output$estimated_trend[which(output$ts_start==41)] = -1 * output$estimated_trend[which(output$ts_start==41)]
output$ts_start=as.factor(output$ts_start)
Y = sim$y_sim
Y = t(scale(t(Y)))
ts2 = data.frame(x = 1:100, y = Y[2,])
ggplot(output, aes(x,y=estimated_trend,group=ts_start,col=ts_start)) + geom_line(size=2, alpha=0.7) + 
  #scale_color_viridis(end=0.8,discrete = TRUE) + xlim(51,100) + 
  xlab("Time") + 
  ylab("Estimated time series (# 2)") +theme_bw()

## ----echo=FALSE, results='hide', include=FALSE--------------------------------
L = as.data.frame(t(l))
L$trend = as.factor(seq(1:nrow(L)))
plot_data <- reshape2::melt(L,id.var="trend")
plot_data$x = as.numeric(substr(plot_data$variable, 2, length(plot_data$variable)))
plot_data$x = seq(1,50,5)[11-plot_data$x]
ggplot(plot_data, aes(x=x,y=value,group=trend,colour=trend)) +
  geom_point()+
  geom_line() + xlab("Time") #+ scale_color_viridis(end=0.8, discrete = TRUE)

