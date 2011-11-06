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

###################### OLS ##################


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

###########################################################

#######      Within Group Estimator  

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

#########################################################

######### LSDV Least Squares dummy variable for fixed effects


lsdv.reg <- function(x,y){

  x.lsdv <- as.matrix(cbind(I.n, %x% e, x))

  b.lsdv <- solve(t(x.lsdv) %x% x.lasdv)%*% (t(x.lasdv) %*% y)

  e.lsdv <- y - x.lsdv %*% b.lsdv
  #rediduals
  ssq.lsdv <- as.numeric((t(e.lsdv) %*% e.lsdv) / (nrow(x.lsdv) - ncol(x.lasdv)))
  varcov.lsdv <- ssq.lsdv* (solve(t(x.lsdv) %*% x.lsdv) %*% x.lsdv)
  se.lsdv <- sqrt(diag(varcov.lsdv))
  #Stand error

  t.lsdv <- b.lsdv / se.lsdv

  table.lsdv <- cbind(b.lsdv, se.lsdv, t.lsdv)
  print("LSDV Estimates")
  print(round(table.lsdv, 4))
  #note this will print out a dummy variable estimate for each effect, use [ ] to
  #cut down on large tables
}


############################################################
################## Between group estimator

btwn.reg <- function(x,y){
  e.big <- (1/t) * (I.n %x% e)

  mean.x <- t(e.big) %*% y
  mean.y <- t(e.big) %*% x

  b.btwn <- solve(t(meanx) %*% meanx) %*% (t(meanx) %*% meany)
  #coef estimates for between groups

  e.btwn <- meany - meanx %*% b.btwn
  ssq.btwn <- as.numeric(t(e.btwn) %*% e.btwn) / (nrow(meanx) -ncol(meanx))
  varcov.btwn <- ssq.btwn * (solve(t(meanx) %*% meanx))
  se.btwn <- sqrt(diag(varcov.btwn))
  #Standard Error estimates

  t.btwn <- b.btwn / se.btwn

  btwn.table <- cbind(b.btwn, se.btwn, t.btwn)

  print("Between Estimates")
  print(round(btwn.table, 4))
}


  


