lsr::rmAll(F)
working.path = "/home/lee/Rprojects/NLCtrl2/"
try(
  setwd(working.path), silent = T
)


source("R/libNLCtrl.R")
source("R/libRandomHelper.R")
source("R/OO.R")
source("R/libDataLoader.R")
library(functional)



goFun = function(x, vars, play.rounds) {

  NLCommand("setup")

  reports = unique(c("ticks", vars))

  df.col.names = tolower(reports)

  NLDoReport(play.rounds, "go-once", reports,
             as.data.frame = TRUE, df.col.names = df.col.names)
}



# starts here ===============================

num.expt = 16

# setup params list (for every experiments) ====
exptsList = list()
for (i in 1:num.expt) {
  expt = makeBaseParamsSet()

  # 随机选取的变量 ====

  # 0 ~ 19的整数，三角分布，不能等于V = 20
  expt$`v0` = (randomIntRangeTri(0, 19))

  # 20 ~ 100的整数
  expt$`init-wealth` = (randomIntRange(20, 100))

  # 常量/参数 ====
  expt$`misjudge-rate` = 0.05
  expt$`mutation-rate` = 0.001
  expt$`p_res` = 0.5
  expt$`init_pop` = 500
  expt$`max_alpha_beta` = 10
  expt$`min_alpha_beta` = 1
  expt$`v` = 20
  expt$`birth-factor` = 1


  expt$`lock_beta?` = "false"

  exptsList[[i]] = expt
}


# input ====
model.path = ("/home/lee/NetLogo_models/edm.2.6.2.nlogo")

expt.name = "expt_05"

cores = detectCores()

vars = getGlobalsFromFile(model.path);
vars

play.rounds = 50000

# run! ====
rest = doExperiments(model.path, exptsList,
                     goFun, cores = cores,
                     expt.name = expt.name,
                     vars = vars,
                     play.rounds = play.rounds)


print(rest)

expt.result = loadExpt(expt.name)

print(expt.result)

# cut ==========
ticks = c(1, seq(100, 50000, 100))

cutExptResult = function(x, ticks) {
  expt.cutted = lapply(x, function(xx) {
    xx[xx$ticks %in% ticks,]
  })
  class(expt.cutted) = class(x)
  expt.cutted
}

expt.cutted = cutExptResult(expt.result, ticks)


expt.cutted


# expt05.result ====
expt05.result = loadExpt("expt_05")
length(expt05.result)

getherParam(expt05.result, "v0") %>% hist(breaks = 21)
