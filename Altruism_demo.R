lsr::rmAll(F)


isWin = function(){
  return(Sys.info()['sysname'] == "Windows")
}

if(isWin()){
  working.path = "C:/Users/lee/Rprojects/NLCtrl2"
  nl.path = 'C:/Program Files/NetLogo 6.0.4/app'
}else{
  working.path = "/home/lee/Rprojects/NLCtrl2"
  nl.path = '/opt/NetLogo 6.0.4/app'
}
   
try(
  setwd(working.path), silent = T
)

source("R/libNLCtrl.R")
source("R/libRandomHelper.R")
source("R/OO.R")
source("R/libDataLoader.R")
source("R/utils.R")
source("R/libDataManipulation.R")
source("R/theme_media.R")





goFun = function(x, vars, play.rounds) {

  NLCommand("setup")

  reports = unique(c("ticks",'count patches with [pcolor = pink]'))

  df.col.names = c('ticks','altruists')

  NLDoReport(play.rounds, "go", reports,
             as.data.frame = TRUE, df.col.names = df.col.names)
}



# starts here ===============================

num.expt = 20

# setup params list (for every experiments) ====
exptsList = list()
for (i in 1:num.expt) {
  expt = makeBaseParamsSet()

  expt$`benefit-from-altruism` = randomIntRange(0,90) / 100

  exptsList[[i]] = expt
}


# input ====
model.path = file.path(nl.path,"models/Sample Models/Biology/Evolution/Altruism.nlogo")

expt.name = "Altruism_demo"

cores = 2 #detectCores()

vars = getGlobalsFromFile(model.path);
vars

play.rounds = 500

output.dir = file.path(working.path,'output')

# run! ====
rest = doExperiments(model.path, 
                     exptsList,
                     goFun, 
                     nl.path = nl.path,
                     cores = cores,
                     expt.name = expt.name,
                     vars = vars,
                     output.dir = output.dir,
                     play.rounds = play.rounds)


print(rest)

expt.result = loadExpt(expt.name,output.dir = output.dir)

print(expt.result)



# plot ===================

altruists = getherVariable(expt.result,"altruists")
benefit = getherParam(expt.result, "benefit-from-altruism")

df = data.frame(benefit,altruists = t(altruists[nrow(altruists),]))

qplot(benefit,altruists,data=df) + theme_media()

qplot(getTicks(altruists), altruists$altruists_20, geom="line") + 
  theme_media() + xlab("ticks") + ylab("altruists") 












