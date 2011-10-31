#writing our own ols regression function
## should probably rename this

data(women)
x <- as.matrix(women$height)
y <- as.matrix(women$weight)
e <- as.matrix(1,15)
x <- cbind(e,x)
# example data, shoul try it with others.

ols.reg <- function(x,y){
  
  coef <- solve(t(x)%*%x%*%t(x)%*%y)
  print("coef estimates")
  return(coef)
  print(coef)
}

#add standard errors, p, t scores. 

