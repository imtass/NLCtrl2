v0 = getherParam(expt.cutted,"v0")
init_wealth = getherParam(expt.cutted,"init-wealth")
source("viewModels.R")
library(stargazer)
library(latex2exp)

head(cs)

ticks = c(100,1000,10000,50000)
lv0 = log(v0)
lv0[lv0==-Inf] = NA

li = log(init_wealth)

# edm ============
edm.cs = getCrossSection(edm,ticks)

ret = lapply(1:4,function(i){
  lm(edm.cs[,i] ~ lv0 )
})
#printModels(ret,no.space = T,column.sep.width = "2pt")
viewModels(ret)


# alpha ============
alpha.cs = getCrossSection(alpha,ticks)

ret = lapply(1:4,function(i){
  lm(alpha.cs[,i] ~ lv0 )
})

viewModels(ret)


# beta ============
beta.cs = getCrossSection(beta,ticks)

ret = lapply(1:4,function(i){
  lm(beta.cs[,i] ~ lv0)
})

viewModels(ret)

# dcost ============
dcost.cs = getCrossSection(d_cost_mean,ticks)
ret = lapply(1:4,function(i){
  lm(dcost.cs[,i] ~ lv0  )
})

viewModels(ret)

# acost ============
acost.cs = getCrossSection(a_cost_mean,ticks)
ret = lapply(1:4,function(i){
  lm(acost.cs[,i] ~ lv0)
})

viewModels(ret)

# 5w代：合并在一起回归 ====

edm.5w = getCrossSection(edm,50000)
lm1 = lm(edm.5w[,1] ~ v0)

plot2pdf("edm_v0",function(x){
  par(mar=c(4,4,1,1),lwd = 1,col="black")
  plot((edm.5w[,1]) ~ (v0),ylim = c(0,5),xlab=TeX("$V_0$"),ylab=TeX("$Gamma$"))
  abline(lm1)
})

edm.1w = getCrossSection(edm,10000)
lm1.1 = lm(edm.1w[,1] ~ v0)

plot2pdf("edm_v0_1w",function(x){
  par(mar=c(4,4,1,1),lwd = 1,col="black")
  plot((edm.1w[,1]) ~ (v0),ylim = c(0,5),xlab=TeX("$V_0$"),ylab=TeX("$Gamma$"))
  abline(lm1.1)
})



# ===============
alpha.5w = getCrossSection(alpha,50000)
lm2 = lm((alpha.5w[,1]) ~ v0)

plot2pdf("alpha_v0",function(x){
  par(mar=c(4,4,1,1),lwd = 1,col="black")
  plot((alpha.5w[,1]) ~ (v0),ylim = c(0,10),xlab=TeX("$V_0$"),ylab=TeX("$Alpha$"))
  abline(lm2)
})


plot(density((alpha.5w[,1])),ylim=c(0,0.8),lty=3)

plot2pdf("dens_alpha_5_15",function(x){
  par(mar=c(2,2,1,1),lwd = 1,col="black")
  
  df = data.frame(a = (alpha.5w[,1]),(v0))
  plot(density(filter(df,v0>15)[,1]),lty=2,ylim=c(0,0.8),main = "",ylab="")
  lines(density(filter(df,v0<=5)[,1]),lty=1)
  legend(6, 0.75, legend=c(TeX("$V_0 > 15$"),TeX("$V_0 \\leq 5$")),
         col="black", lty=c(2,1), cex=1.5,bty = "n")
})



# ===============

beta.5w = getCrossSection(beta,50000)
lm3 = lm((beta.5w[,1]) ~ v0)

library(latex2exp)

plot2pdf("beta_v0",function(x){
  par(mar=c(4,4,1,1),lwd = 1,col="black")
  plot((beta.5w[,1]) ~ (v0),ylim = c(0,10),xlab=TeX("$V_0$"),ylab=TeX("$Beta$"))
  abline(lm3)
})


plot2pdf("dens_beta_5_15",function(x){
  par(mar=c(2,2,1,1),lwd = 1,col="black")
  
  df = data.frame(a = (beta.5w[,1]),(v0))
  plot(density(filter(df,v0>15)[,1]),lty=2,ylim=c(0,1.5),main = "",ylab="")
  lines(density(filter(df,v0<=5)[,1]),lty=1)
  legend(6, 1.4, legend=c(TeX("$V_0 > 15$"),TeX("$V_0 \\leq 5$")),
         col="black", lty=c(2,1), cex=1.5,bty = "n")
})


# =============
acost.5w = getCrossSection(a_cost_mean,50000)
lm4 = lm(acost.5w[,1] ~ v0)

plot2pdf("acost_v0",function(x){
  par(mar=c(4,4,1,1),lwd = 1,col="black")
  plot((acost.5w[,1]) ~ (v0),xlab=TeX("$V_0$"),ylab=TeX("$C_2$"))
  abline(lm4)
})

dcost.5w = getCrossSection(d_cost_mean,50000)
lm5 = lm(dcost.5w[,1] ~ v0)

plot2pdf("dcost_v0",function(x){
  par(mar=c(4,4,1,1),lwd = 1,col="black")
  plot((dcost.5w[,1]) ~ (v0),xlab=TeX("$V_0$"),ylab=TeX("$C_1$"))
  abline(lm5)
})

viewModels(lm1,lm2,lm3,lm4,lm5)
printModels(lm1,lm2,lm3,lm4,lm5,no.space = T,column.sep.width = "1pt")




plot((beta.5w[,1]) ~ (v0))
abline(lm3)








