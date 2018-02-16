setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/clustering exercise')
#setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/edinburgh series')


data<-read.csv("data.csv", header = FALSE)
data3<-data
#for(i in 1:ncol(data3)){
#  max_data3<-max(data3[,i])
#  data2<-cbind(data2,data3[,i]/max_data3)
#}
#for(i in 1:ncol(data3)){
#  temp<-min(data3[,i])
#  max_data3<-max(data3[,i])
#  data2<-cbind(data2,(data3[,i]-temp)/(max_data3-temp))
#}
#for (i in 1:ncol(data3)){
#  temp1<-mean(data3[,i])
#  temp2<-sd(data3[,i])
#  data2<-cbind(data2,(data3[,i]-temp1)/temp2)
#}
#data1 <- as.matrix(data3)
data1<-data
sil <- rbind()
#i=5
for (i in 2:15){
  #i=15
  p<-pam(data1,i)
  km<-kmeans(data1,i)

  ss <- function(data1) sum(scale(data1, scale = FALSE)^2)
  withinss <- sapply(split(as.data.frame(data1), p$clustering), ss)
  p1<-list(cluster=p$clustering,centers=p$medoids,withins=withinss,size=p$clusinfo[,1])

  #output<-clustIndex(p1,data1,index="calinski")
  #output<-clustIndex(p1,data1,index="hartigan")
  #output<-clustIndex(p1,data1,index="marriot")
  #output<-clustIndex(p1,data1,index="tracew")
  #output<-clustIndex(p1,data1,index="scott")

  #output<-clustIndex(km,data1,index="marriot")
  #output<-clustIndex(km,data1,index="hartigan")
  #output<-clustIndex(km,data1,index="calinski")
  output<-clustIndex(km,data1,index="tracew")
  #output<-clustIndex(km,data1,index="scott")
  sil<-rbind(sil,output)
}
d<-sil
k<-cbind(0)
k<-cbind(k,0)
for (i in 2:14){
  k<-cbind(k,(d[i+1]+d[i-1]-2*d[i]))
  #k<-cbind(k,(d[i]-d[i-1]))
}
plot(k[,1:14],type="o",col="blue")
min(k)
which.min(k[,1:13])
which.max(k[,1:13])
#intVal(pm,data1,index="marriot")