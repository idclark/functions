#writing our own ols regression function


#Example data############################################################
data(women)
x <- as.matrix(women$height)
y <- as.matrix(women$weight)
# some useful variables and matrices

n <-
t <-
nt <- n * 
# temporal element, how many units  and time observations

I.t <- diag(1,t,t)
# Identity matrix for t

e <- matrix(1,15)
#

J.t <-  e %*% t(e)

qmat <- I.t - 1 / t * J.t

I.nt <- diag(1, n*t , n*t)

I.n <- diag(1,n,n)

k <- ncol(x[-1])

x <- cbind(e,x)
# example data. You need to cbind() vector of ones
# for global intercept for functions below. hence cbind(e,x) now have a matrix
#of explamataory variables and global intercept.



ols.reg <- function(x,y){
#oridinary least aquares regression 
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

within.fix <- function(x,y){
  #within group estimator for fixed effects
  inxx.w <- solve(t(x) %*% qmatall %*% x)
  b.w <- inxx.w %*% t(x) %*% qmatall %*% y

  y.tilde <- as.matrix(qmatall %*% y)
  x.tilde <- as.matrix(qmatall %*% x)
  #FE Standard errors

  e.tilde <- as.matrix(y.tide - (x.tilde %*% b.w))
  ssq.fe <- (t(e.tilde) %*% e.tilde) / (nt- n- k )
  se.w <- sqrt(diag(as.numeric( ssq.fe) * inxx.w))
  t.w <- b.w / se.w

  fetable <- cbind(b.w, se.w, t)
  print("FE Estimates")
  print(round(fetable, 4))
}



