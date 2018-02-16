setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/clustering exercise')
#setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/edinburgh series')

data<-read.csv("data.csv", header = FALSE)
data3<-data

data2<-cbind()
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
#data3<-data2
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
