#writing our own ols regression function

data(women)
x <- as.matrix(women$height)
y <- as.matrix(women$weight)
e <- as.matrix(1,15)
x <- cbind(e,x)

reg <- function(x,y){
  
  coef <- solve(t(x)%*%x%*%t(x)%*%y)
  print("coef estimates")
  return(coef)
  print(coef)
}


  

