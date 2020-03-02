makeTickData = function(df){
  class(df) = c("TickData",class(df))
  df
}

ggplot.TickData = function(x){
  #str(x)
  # ticks = x$ticks
  # data = mutate(x,-ticks)
  df2 = melt(x,id.var="ticks")
  ggplot(df2,aes(x=ticks,y=value,color=variable)) + geom_line() + mytheme_right
}



x = df
class(x)
