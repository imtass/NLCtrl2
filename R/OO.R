library(reshape2)
library(dplyr)
library(ggplot2)
library(triangle)


getTicks = function(x){
  attr(x,"ticks")
}

# plot a gethered var ========================
plotStat = function(x, ...) {
  UseMethod("plotStat", x)
}


plotStat.ExptRslt.Var = function(x) {
  #conf = apply(x[, -1], 1, function(x){t.test(x)$conf.int %>% t() %>% data.frame()})
  #conf = Reduce(rbind,conf)

  stat.sd = apply(x, 1, function(y){sd(y)})
  stat.mean = rowMeans(x)

  ticks = getTicks(x)
  stat = data.frame(ticks, mean = stat.mean, 
                    var1 = stat.mean + stat.sd,
                    var2 = stat.mean - stat.sd,
                    var1 = stat.mean + 2 * stat.sd,
                    var2 = stat.mean - 2 * stat.sd
                    )

  
  dd2 = melt(stat, id.vars = "ticks")

  ggplot(dd2, aes(x = ticks, y = value, group = variable, color = variable)) +
    geom_line(aes(size = variable)) + scale_size_manual(values = c(1,
    0.5, 0.5, 0.5, 0.5)) + theme_bw() + scale_colour_manual(values = c("royalblue4",
    "royalblue2", "royalblue2", "grey", "grey"))
}


#var = apply(x[, -1], 1, function(x){t.test(x)$conf.int %>% t() %>% data.frame()})


# get a cross section data (of ticks) from a ExptRslt.Var =====================
getCrossSection = function(x, ...) {
  UseMethod("getCrossSection", x)
}

getCrossSection.ExptRslt.Var = function(x, ticks) {
  ii = (ticks %in% getTicks(x))

  df = x[getTicks(x) %in% (ticks),] %>% t() %>% as.data.frame()
  names(df) = paste0("t_", ticks)
  df
}

# get one expt from result ==============
getExperiment = function(x, ...) {
  UseMethod("getExperiment", x)
}
getExperiment.ExptRslt = function(x, expt.id, ...) {
  ret = x[[expt.id]]
  class(ret) = c(class(ret), "AnExptData")
  ret
}

# plot expt data =============
plotExptData = function(x, ...) {
  UseMethod("plotExptData", x)
}

plotExptData.AnExptData = function(expt, ...) {
  df = select(expt, ...)
  head(df)

  df.m = melt(df, id.var = "ticks")
  head(df.m)

  p1 = ggplot(df.m, aes(x = ticks, y = value, colour = variable)) +
    geom_line() + # ggtitle(paste0('Data of Expt.', expt.id)) +
  mytheme
  p1
}

# get and print param set===========================
print.ParamsSet = function(x) {
  ret = data.frame(var = names(x), value = unlist(x))
  rownames(ret) = NULL
  # print(filter(ret,var!='random-seed'))
  print(ret)
}


getParamsSet = function(x) {
  attr(x, "paramsSet")
}

# gether all value of a params ====
getherParam = function(x,...){
  UseMethod("getherParam",x)
}

getherParam.ExptRslt = function(x,param.name){
  lapply(x,function(y){
    ps = getParamsSet(y)
    unlist(ps[param.name])
  }) %>% unlist() %>% unname()
}





# print expt oject =============
print.ExptRslt = function(x) {

  cat("\nExperiments Set ID: ",attr(x,"set.id"),"\n")
  cat("Number of Experiments: ", length(x), "\n")
  cat("Ticks of Experiments: ", nrow(x[[1]]), "\n")
  
  cat("Time used: ", attr(x,"Time.used"), "\n")
  cat("Average time used: ", attr(x,"Avg.time.used"), "\n")

  cat("\nVariables: \n")
  print(colnames(x[[1]]))
}






# ggplot a sample dataframe with a id.var ====
plotdf = function(x,id.var,...){

  x.m = melt(x,id.var)

  p = ggplot(x.m,aes_string(x=id.var,y="value",color="variable"))
  p
}


# get var by range of res_percent ====
getVarByRange = function(exptRslt,var.name,value.range){
  var.range.list = apply(value.range,1,function(x){
    a = x["from"]
    b = x["to"]
    #print(a)
    ss = subset(exptRslt,`res_percent` > a & `res_percent` <= b ,environment())
    ss.var = getherVariable(ss,var.name)
    ret = data.frame(rowMeans(ss.var[,-1]))
    colnames(ret) = paste0("R",a,"_",b)
    ret
  })

  var.range.list = Reduce(cbind,var.range.list)
  var.range.list = data.frame(ticks = 1:nrow(var.range.list),var.range.list)
  var.range.list
}

# plot var by range of res_percent(5 blocks and 10 blocks)
range10 = tibble(from=seq(0,90,10),to=from+10) %>% data.frame()
range5 = tibble(from=seq(0,80,20),to=from+20) %>% data.frame()

# plot10_5 = function(exptRslt,var.name){
#   var.list.10 = getVarByRange(exptRslt,var.name,range10)
#   var.list.5 = getVarByRange(exptRslt,var.name,range5)
#   #alpha.list = data.frame(ticks = 1:nrow(alpha.list),alpha.list)
# 
#   p1 = plotdf(var.list.10,"ticks") + geom_line() +
#     ggtitle(paste0(var.name," (根据res_percent分10组)")) +
#     mytheme_right
# 
#   p2 = plotdf(var.list.5,"ticks") + geom_line() +
#     ggtitle(paste0(var.name," (根据res_percent分5组)")) +
#     mytheme_right
# 
#   multiplot2(p1,p2)
# }
# 

# plot Cross Sections ====
plotCrossSections = function(x,...){
  UseMethod("plotCrossSections",x)
}
plotCrossSections.ExptRslt = function(x,var.name,cs = c(50,120,1000)){
  res_percent = getherParam(x,"res_percent")
  var.cs = getCrossSection(getherVariable(x,var.name),cs)
  names(var.cs) = paste0("t",cs)
  df = data.frame(var.cs,res_percent)

  plotdf(df,"res_percent")+ geom_point(size=1)
}

# pick one number
pickOne = function(choices){
  sample(choices,1)
}
