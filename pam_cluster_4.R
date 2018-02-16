setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/clustering exercise')
#setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/edinburgh series')

data<-read.csv("data.csv", header = FALSE)
data3<-data
#for(i in 1:ncol(data3)){
#  max_data3<-max(data3[,i])
#  data2<-cbind(data2,data3[,i]/max_data3)
#}

#for (i in 1:ncol(data3)){
#  temp1<-mean(data3[,i])
#  temp2<-sd(data3[,i])
#  data2<-cbind(data2,(data3[,i]-temp1)/temp2)
#}
data3<-data
noc<-3
p<-pam(data3,noc)

h<-hclust(dist(data3), method = "median", members = NULL) #complete#median#centroid
cen<-cutree(h,k=noc)#i
#h1<-medoids(dist(data3),cen)

cent<-rbind()
for(k in 1:noc){
  cent <- rbind(cent, colMeans(data3[cen == k, , drop = FALSE]))
}

km<-kmeans(data3,cent)

table(p$clustering)
table(km$cluster)
pamc<-p$clustering
kmc<-km$cluster
kmc11 <- as.matrix(kmc)
data_num<-rbind()
res<-matrix(0,nrow(kmc11),1)
count<-matrix(0,noc,1)
data_cls<-rbind()

count1<-matrix(0,noc,noc)
for (i in 1:noc){
  for (j in 1:noc){
    for (k in 1:nrow(kmc11)){
      if (kmc[k]==i && pamc[k]==j)
        count1[i,j]<-count1[i,j]+1
    }
  }
}
count1

#count2<-rbind()
#for (i in 1:nrow(data3)){
#  c_test<-cor.test(data3[i,],d2)
#  count2<-rbind(count2,c_test$estimate)
#}

for (i in 1:nrow(kmc11)){
  if (kmc[i]==1 && pamc[i]==3){#6
    res[i]<-1
    count[1]<-count[1]+1
    temp<-c(i,1)
    #data_cls<-head(do.call(rbind,temp))
    data_cls<-rbind(data_cls,temp)
  }
  if (kmc[i]==2 && pamc[i]==1){#2
    res[i]<-2
    count[2]<-count[2]+1
    temp<-c(i,2)
    data_cls<-rbind(data_cls,temp)
  }
  if (kmc[i]==3 && pamc[i]==2){#8
    res[i]<-3
    count[3]<-count[3]+1
    temp<-c(i,3)
    data_cls<-rbind(data_cls,temp)
  }
#  if (kmc[i]==4 && pamc[i]==2){#4
#    res[i]<-4
#    count[4]<-count[4]+1
#    temp<-c(i,4)
#    data_cls<-rbind(data_cls,temp)
#  }
}

temp<-c(10000,100000)
data_cls<-rbind(data_cls,temp)

data4<-rbind()
k<-1
for (i in 1:nrow(data3)){
  if(data_cls[k,1]==i){
    data4<-rbind(data4,c(data3[i,],data_cls[k,2]))
    k<-k+1
  }
  else
    data4<-rbind(data4,c(data3[i,],'NaN'))
}

write.csv(data4,file="output_4cls.csv")#,row.names = FALSE,col.names = FALSE)

res<-rbind()
for (i in 1:nrow(data_cls)){
  if (data_cls[i,2]==4)
    res<-rbind(res,data3[data_cls[i,1],])
}

#res1<-res*1
colors<-c(rep("lavender",10))#,las=2,names=c("ER","PgR","CK7/8","CK5/6","EGFR","HER2","HER3","HER4","p53","MUC1"),col=colors)
boxplot(data3,ylim=c(0,1),las=2,names=c("ER","PgR","CK7/8","CK5/6","EGFR","HER2","HER3","HER4","p53","MUC1"),col=colors)
#axis(side = 2, xaxp=c(0,0.5,1)


data5<-rbind()
data6<-rbind()
count1<-0
for (i in 1:nrow(res)){
  if(res[i,10]>.6){
    count1<-count1+1
    data6<-rbind(data6,c(res[i,],4))
  }
  else
    data5<-rbind(data5,res[i,])
}

res1<-rbind()
for (i in 1:nrow(data6)){
  if (data6[i,12]==4)
    res1<-rbind(res1,data6[i,1:11])
}

colors<-c(rep("lavender",10))#,las=2,names=c("ER","PgR","CK7/8","CK5/6","EGFR","HER2","HER3","HER4","p53","MUC1"),col=colors)
boxplot(data5,ylim=c(0,1),las=2,names=c("ER","PgR","CK7/8","CK5/6","EGFR","HER2","HER3","HER4","p53","MUC1","Ki67"),col=colors)
#axis(side = 2, xaxp=c(0,0.5,1)