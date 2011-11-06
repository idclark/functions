#writing our own ols regression function


#Example data############################################################
data(women)
x <- as.matrix(women$height)
y <- as.matrix(women$weight)
e <- matrix(1,15)
x <- cbind(e,x)
# example data. You need to cbind() vector of ones
# for global intercept for functions below. 

ols.reg <- function(x,y){
  
  coef <- solve(t(x) %*% x) %*% (t(x) %*% y)
  
  e.ols <- y - x %*% coef
  ssq.ols <-  (t(e.ols) %*% e.ols) / (nrow(coef) - ncol(x))
  varcov.ols <- ssq.ols * (solve(t(x) %*% x))
  se.ols <-  sqrt(diag(varcov.ols))
  
  t <- coef / se.ols
  
  table <- cbind(coef, se.ols, t)
  print("OLS Estimates")
  print(round(table,4))  
}





