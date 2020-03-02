source("R/libDataLoader.R")
source("R/libDataManipulation.R")
source("R/libPlotContour.R")
source("R/OO.R")

library(MyPlot)
library(MyUtils)
#library(data.table)
#library(dplyr)
library(tidyverse)
library(HDInterval)

lastRow = function(x){
  x[nrow(x),] %>% t() %>%  as.vector()
}

firstRow = function(x){
  x[1,] %>% t() %>%  as.vector()
}

getRowByTicks = function(x, ticks){
  ii = which(ticks == getTicks(x))
  x[ii,] %>% t() %>% as.vector()
}

# saveRDS(expt.cutted,"expt05.cutted.RDS")
# expt.cutted = readRDS("expt05.cutted.RDS")



expt.cutted 

alpha = getherVariable(expt.cutted,"alpha_mean")
beta = getherVariable(expt.cutted,"beta_mean")
edm = getherVariable(expt.cutted,"edm_mean")

par(mar=c(2,2,1,1),lwd = 1,col="black")

# 主要变量分布的等高线 =============
plotContour(alpha,levels.breaks = 60)
plotContour(beta,levels.breaks = 30)
plotContour(edm,value.range = c(0,3.5),levels.breaks = 35)

mean(lastRow(edm));median(lastRow(edm))
hdi(density(lastRow(edm)),credMass = 0.05) %>% round(3)

mean(lastRow(alpha));median(lastRow(alpha))
hdi(density(lastRow(alpha)),credMass = 0.05) %>% round(3)

mean(lastRow(beta));median(lastRow(beta))
hdi(density(lastRow(beta)),credMass = 0.05) %>% round(3)

# 截面分布图2 ===============

plotDist = function(x,ticks ,xlim = NULL,ylim=NULL){
  ret = lapply(ticks, function(tt){
    density(getRowByTicks(x,tt))
  })
  
  plot(ret[[1]],main="",xlim=xlim,ylim = ylim,lty=4)
  #lines(b,col='red')
  lines(ret[[2]],lty=2)
  #lines(dd,col="green")
  lines(ret[[3]],lty=1)
  
}

plotDist(edm,c(100,2000,50000),xlim=c(0,3.5))
legend(2.2, 2.4, legend=c("t = 100", "t = 2000","t = 50000"),
       col="black", lty=c(4,2,1), cex=1.5,bty = "n")

plotDist(alpha,c(100,2000,50000),xlim=c(0,10))
legend(6.5, 0.48, legend=c("t = 100", "t = 2000","t = 50000"),
       col="black", lty=c(4,2,1), cex=1.5,bty = "n")
abline(v=2.4)


plotDist(beta,c(100,2000,50000),xlim=c(0,10),ylim=c(0,0.5))
legend(6.5, 0.48, legend=c("t = 100", "t = 2000","t = 50000"),
       col="black", lty=c(4,2,1), cex=1.5,bty = "n")

abline(v=1.4)



# 查看V0的分布
v0 = getherParam(expt.cutted,"v0")
gghist(v0,bins = 20) + mytheme 


# fight_r_p ====
fight_r_p =  getherVariable(expt.cutted,"fights_real_p" )
plotStat(fight_r_p)
plotContour(fight_r_p,value.range = c(0,0.5),levels.breaks = 5)
plotDist(fight_r_p,c(100,2000,50000),xlim=c(0,0.5),ylim=c(0,20))
legend(0.3, 19, legend=c("t = 100", "t = 2000","t = 50000"),
       col="black", lty=c(4,2,1), cex=1.5,bty = "n")
hdi(density(lastRow(fight_r_p)),credMass = 0.05) %>% round(3)

# retreats_d_p ====
retreats_d_p =  getherVariable(expt.cutted,"retreats_d_p" )
plotStat(retreats_d_p)
plotContour(retreats_d_p,value.range = c(0,0.1),levels.breaks = 10)
plotDist(retreats_d_p,c(100,2000,50000),xlim=c(0,0.1),ylim=c(0,250))
legend(0.06, 240, legend=c("t = 100", "t = 2000","t = 50000"),
       col="black", lty=c(4,2,1), cex=1.5,bty = "n")
hdi(density(lastRow(retreats_d_p)),credMass = 0.5) %>% round(5)

# retreats_a_p ====
retreats_a_p =  getherVariable(expt.cutted,"retreats_a_p" )
plotStat(retreats_a_p)
plotContour(retreats_a_p,value.range = c(0,1),levels.breaks = 10)
plotDist(retreats_a_p,c(100,2000,50000),xlim=c(0,1),ylim=c(0,20))
legend(0.4, 19, legend=c("t = 100", "t = 2000","t = 50000"),
       col="black", lty=c(4,2,1), cex=1.5,bty = "n")
hdi(density(lastRow(retreats_a_p)),credMass = 0.05) %>% round(3)

# a_cost_mean ====
a_cost_mean =  getherVariable(expt.cutted,"a_cost_mean" )
plotStat(a_cost_mean)
plotContour(a_cost_mean,value.range = c(0,5),levels.breaks = 10)
hist(lastRow(a_cost_mean),breaks = 100)

# d_cost_mean ====
d_cost_mean =  getherVariable(expt.cutted,"d_cost_mean")
plotStat(d_cost_mean)
plotContour(d_cost_mean,value.range = c(0,100),levels.breaks = 50)
hist(lastRow(d_cost_mean),breaks = 100)







