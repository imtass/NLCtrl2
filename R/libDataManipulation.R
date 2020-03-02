# gether a var from the result ====
getherVariable = function(x, ...) {
  UseMethod("getherVariable", x)
}
## 中文中文
## abababab

getherVariable.ExptRslt = function(x, var.name, condition) {

  var.name.list = lapply(x, function(y) {
    colnames(y)
  }) %>% unique()

  if (!var.name %in% var.name.list[[1]]) {
    stop(paste0("No such variable : ", var.name))
  }

  if (missing(condition)){
    data = x
  }else{
    data = subset_.ExptRslt(x,substitute(condition))
  }

  ret = lapply(data, function(y) {
    y[var.name]
  })

  ret2 = Reduce(cbind, ret) 
  names(ret2) = names(ret2) %+% "_" %+% 1:ncol(ret2)
  ret2 = as_tibble(ret2)
  #colnames(ret2) = paste(var.name, (1:ncol(ret2)), sep = "_")
  #ret3 = data.frame(ticks = x[[1]][, 1], ret2)

  attr(ret2,"ticks") = x[[1]]$ticks
  attr(ret2,"mean") = rowMeans(ret2)
  attr(ret2,"sd") = apply(ret2, 1, sd)
  attr(ret2,"var.name") = var.name

  class(ret2) = c("ExptRslt.Var",class(ret2))
  ret2
}

# subseting a experiment by parames ====
subset.ExptRslt= function(x,subset,env=parent.frame()){
  e = substitute(subset)
  res = sapply(x,function(y){
    params = getParamsSet(y)
    eval(e,params,env)
  })

  res2 = x[res]
  class(res2) = c("ExptRslt",class(res2))
  res2
}


# inner function of subset.ExptRslt
subset_.ExptRslt= function(x,e,env=parent.frame()){
  #e = substitute(subset)
  res = sapply(x,function(y){
    params = getParamsSet(y)
    eval(e,params,env)
  })

  res2 = x[res]
  class(res2) = c("ExptRslt",class(res2))
  res2
}



