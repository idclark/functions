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


################################################################

########## Random Effects Model

random.reg <- function(x,y){
  
# estimate sigma^2 u from within group residuals
 
inxx.w <- solve(t(x) %*% qmatall %*% x)
b.w <- inxx.w %*% t(x) %*% qmatall %*% y
y.tilde <- as.matrix(qmatall %*% y)
x.tilde <- as.matrix(qmatall %*% x)
e.tilde <- as.matrix(y.tilde - (x.tilde %*% b.w))

var.u <- as.numeric((t(e.tilde) %*% e.tilde)/(n*(t-1)-k))     

varcov.u <- (var.u)*as.matrix(inxx.w)  ### for use in Hausman test

# estimate sigma^2 alpha from between group residuals

e.big <- (1/t)*(I.n %x% e)
meany <- t(e.big)%*%y
meanx <- t(e.big)%*%xc
b.btwn <- solve(t(meanx)%*%meanx)%*%(t(meanx)%*%meany)
e.btwn <- meany - meanx %*% b.btwn

var.a <- as.numeric((t(e.btwn) %*% e.btwn)/(n-(k+1))) - (1/t)*var.u  

# calculate psi, V^-1, and omega^-1

psi <- as.numeric(var.u/(var.u + t*var.a))

Vinv <- (1/var.u)*(I.t - (1/t)*J.t + psi*((1/t)*J.t))

omegainv <- I.n %x% Vinv

# calculate RE estimates and se's

delta.re <- solve(t(xc)%*%omegainv%*%xc)%*%(t(xc)%*%omegainv%*%y) 
T.xx <- t(xc)%*%xc
B.xx <- (1/t)*(t(xc)%*%(I.n%x%J.t)%*%xc)
varcov.re <- var.u*solve((T.xx-B.xx)+psi*B.xx)
se.re <- as.numeric(sqrt(diag(varcov.re)))
t.re <- delta.re/se.re

reresults <- round(cbind(delta.re,se.re,t.re),3)
rownames(reresults) <- c("constant",varnames)
colnames(reresults) <- c("b.re","se.re","t.re")
reresults
}


############
######   Hausman Test - optional / rough code 
# Hausman test

b.re <- delta.re[-(1:1)]
m <- t(b.w-b.re)%*%solve(varcov.u-varcov.re[2:10,2:10])%*%(b.w-b.re)
m

########################################

######### Panel Corrected Standard Errors.

### PCSEs are used with OLS betas so i included b.ols again

ols.pcse <- function(x,y){

  b.ols <- solve(t(x)%*%x)%*%(t(x)%*%y)
  e.ols <- y -  %*% b.ols

  E <- matrix(e.ols,t,n)
  sigma <- (t(E)%*%E)/t
  omega <- sigma %x% I.t

  varcov.pcse <- solve(t()%*%x)%*%t(x)%*%omega%*% x %*% solve(t(x)%*%x)
  se.pcse <- as.numeric(sqrt(diag(varcov.pcse)))
  t.pcse <- b.ols/se.pcse

  pcseresults <- round(cbind(b.ols,se.pcse,t.pcse),3)
  rownames(pcseresults) <- c("constant",varnames)
  colnames(pcseresults) <- c("b.ols","se.pcse","t.pcse")
  pcseresults
}




