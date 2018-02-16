setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/dbscan')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/dbscan')

library("subspace")
library("mlbench")
data("Ionosphere")
x=Ionosphere[,3:34]
clust<-CLIQUE(x, xi = 10, tau = 0.2)
