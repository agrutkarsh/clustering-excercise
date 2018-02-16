setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/dbscan')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/dbscan')

source('Number_of_Neighbours.R')
#library("mlbench")
#data("iris")
data <- read.csv('mydata.csv', header = FALSE)
data1<-rbind(data[1:100,],data[900:999,])
data <- data1
e <- 0.1  ## distance threshold to find core data points

x <- data
n <- nrow(x) ##number of data points

# num_of_neigh <- matrix(0,n,1)
# D <- matrix(1000,n,n)
# for (i in 1:n){
#   #num_of_neigh[i] <- Number_of_Neighbours(i, x, e)
#   for (j in 1:n){#i:n
#     if (i==j)
#       D[i,j]<-1000
#     else
#       D[i,j] <- sqrt(sum((x[i,] - x[j,]) ^ 2))
#   }
#   temp <- which(D[i,]<=e)
#   num_of_neigh[i]<- length(temp)
# }
# core_points<-which(num_of_neigh>=4)
# core_points<-14

for (i in 1:n){
  min_dist_index<-which.min(D[14,])
  
}