
show_area<-function(){
  library(ggplot2)
  w = timeslow
  t<-0
  num<-0
  group<-""
  
  for(i in 0:23) {
    t<-c(t,rep(i,ncol(w)-1))
    group<-c(group,names(w[,-1]))
    num<-c(num,as.matrix(w[,-1])[i+1,])
  }
  data<- data.frame(Time=t,CrTime=group,Ratio=num)
  data = data[-1,]
  data$CrTime = factor(data$CrTime, levels=c('gt10','gt5le10','gt3le5','gt1le3','le1'))
  #ggplot(data = data, mapping = aes(x = factor(Time), y = Ratio,fill = CrTime)) + geom_bar(stat = 'identity', position = 'dodge')
  #ggplot(data = data, mapping = aes(x = Time, y = Ratio,fill = CrTime)) + geom_bar(stat = 'identity', position = 'stack')
  ggplot(data,aes(x=Time,y=Ratio,group=CrTime,fill=CrTime))+geom_area(position = 'fill')
}
