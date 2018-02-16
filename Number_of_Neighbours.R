Number_of_Neighbours <- function(i, x, e) {
  
  D <- matrix(0,nrow(x),1)
  for (j in 1:nrow(x)){
    D[j] <- dist(rbind(x[1,],x[j,]))
    #sqrt(sum((x[1,] - x[j,]) ^ 2))
  }
  temp <- which(D<=e)
  return(length(temp))
}