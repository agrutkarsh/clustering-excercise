#setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/dbscan')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/dbscan')
library("denpro")
dendat <- read.csv('mydata.csv', header = FALSE)
dendat<-sim.data(n=200,type="mulmod") # data
pcf<-pcf.kern(dendat,h=1,N=c(32,32)) # kernel estimate
lst<-leafsfirst(pcf) # level set tree
td<-treedisc(lst,pcf,ngrid=60) # pruned level set tree
plotvolu(td) # volume plot
plotbary(td)
