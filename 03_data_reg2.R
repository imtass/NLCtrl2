edm.5w = getCrossSection(edm,50000)
lm1 = lm(edm.5w[,1] ~ init_wealth)

alpha.5w = getCrossSection(alpha,50000)
lm2 = lm(alpha.5w[,1] ~ init_wealth)

beta.5w = getCrossSection(beta,50000)
lm3 = lm(beta.5w[,1] ~ init_wealth)

acost.5w = getCrossSection(a_cost_mean,50000)
lm4 = lm(acost.5w[,1] ~ init_wealth)

dcost.5w = getCrossSection(d_cost_mean,50000)
lm5 = lm(dcost.5w[,1] ~ init_wealth)

viewModels(lm1,lm2,lm3,lm4,lm5)
printModels(lm1,lm2,lm3,lm4,lm5,no.space = T,column.sep.width = "1pt")


