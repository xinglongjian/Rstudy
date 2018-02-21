
show_bar<-function(){
  library(ggplot2)
  x <- rep(1:5, each = 3)
  y <- rep(c('A','B','C'),times = 5)
  set.seed(1234)
  z <- round(runif(min = 10, max = 20, n = 15)) 
  df <- data.frame(x= x, y = y, z = z)
  df$y = factor(df$y, levels=c('B','C','A')) 
  #ggplot(data = df, mapping = aes(x = factor(x), y = z,fill = y)) + geom_bar(stat = 'identity', position = 'dodge')
  ggplot(data = df, mapping = aes(x = factor(x), y = z,fill = y)) + geom_bar(stat = 'identity', position = 'stack')
  #ggplot(df,aes(x=x,y=z,group=y,fill=y))+geom_area(position = 'fill')
}
