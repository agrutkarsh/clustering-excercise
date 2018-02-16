setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/dbscan')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/dbscan')

library("dbscan")

library("mlbench")
data("Ionosphere")
x=Ionosphere[,3:34]
clust<-dbscan(x,1.2, minPts = 5)
clust$cluster

count=matrix(0,2,1)
for (i in 1:351){
  if (clust$cluster[i]==1 && Ionosphere[i,35]=='good')
    count[1]=count[1]+1
  if (clust$cluster[i]==2 && Ionosphere[i,35]=='bad')
    count[2]<-count[2]+1
}

data("Sonar")
x<-as.data.frame(Sonar[,1:60])
clust<-dbscan(x, 1.0, minPts = 7)
clust$cluster

data<-read.csv('already_normalised_short_distance.csv', header = FALSE)
x<-data[,c(1:4)]
clust<-dbscan(x, 0.001, minPts = 5)
clust$cluster

count<-matrix(0,5,1)
for (j in 0:4){
  for(i in 1:length(clust$cluster)){
    if (clust$cluster[i]==j){
      count[j+1]<-count[j+1]+1
    }
  }
}
