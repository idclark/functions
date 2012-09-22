
gibbs <- function(N, n.thin){
  #needs to be altered for various situations
  #can also be a data.frame, or what ever params we're sampling from
  data.matrix <- matrix(0, ncol=4, nrow=N)
  data.matrix[,1] <- 1:N
  x <- 0
  x1 <- 0
  y <- 0
  for (i in 1:N){
    for(j in 1:n.thin){
      #random data part
      x <- rgamma(1,3, y*y+4)
      x1 <- rgamma(1,2, y**2)
      y <- rnorm(1, 1/(x+1), 1/sqrt(2*x+2))
      }
     data.matrix[i, 2:4] <- c(x, x1 ,y)
    }
  data.matrix <- data.frame(data.matrix)
  #comment out if you don't want the who sim barfed to console
  names(data.matrix) <- c("Iter", "x","x1" , "y")
  data.matrix
}

#can then do some pretty printing, sanity check plots etc

