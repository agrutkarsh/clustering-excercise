setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/dbscan')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/dbscan')

data <- read.csv('mydata.csv', header = FALSE)
library("ggplot2")
ggplot(data, aes(x = V1, y = V2)) + 
geom_tile()+ 
stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
scale_fill_gradient2(low = "white", high = "red")
