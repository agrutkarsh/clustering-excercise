#setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/dbscan')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/dbscan')

distance <- function(p1,p2) {
  return(sqrt(sum((p1 - p2) ^ 2)))
}

region <- function(i, x, e){
  seed<-cbind()
  for (j in 1:nrow(x)){
    if (distance(x[i,],x[j,])<e){
      seed<-cbind(seed,j)
    }
  }
  return(seed)
}

expand_cluster <- function(x, classifications, i, cid, e, min_pts){
  seeds<-region(i, x, e)
  if (length(seeds)<min_pts){
    classifications[i] <- -1
    return(list("i1"=FALSE,"i2"=classifications))
  } else{
      classifications[i]<-cid
      for (j in 1:length(seeds)){
        temp<-seeds[j]
        classifications[temp]<-cid
      }
      while(length(seeds)>0){
        cp<-seeds[1]
        res<-region(cp, x, e)
        if (length(res)>min_pts){
          for (j in 1:length(res)){
            temp_res<-res[j]
            if (classifications[temp_res]==0 || classifications[temp_res]==-1){
              if (classifications[temp_res]==0){
                seeds<-append(seeds,temp_res)
              }
              classifications[temp_res]<-cid
            }
          }
        }
        if (length(seeds)==1){
          seeds=NULL
        } else{
            seeds<-seeds[2:length(seeds)]
        }
      }
      return(list("i1"=TRUE,"i2"=classifications))
    }
}

#library("mlbench")
#data("iris")
data <- read.csv('mydata.csv', header = FALSE)
#data1<-rbind(data[1:100,],data[900:999,])
#data <- data1
e <- 0.5  ## distance threshold to find core data points
min_pts<-5

x <- data
n <- nrow(x)

cid<-1
classifications<-matrix(0,n,1)
for (i in 1:n){
  #i=1
  point<-x[i,]
  if (classifications[i]==0){
    temp1<-expand_cluster(x, classifications, i, cid, e, min_pts)
    classifications<-temp1$i2
    if (temp1$i1==TRUE){
      cid<-cid+1
    }
  }
}

