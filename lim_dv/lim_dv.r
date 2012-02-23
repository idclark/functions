rm(list=ls())

#Create a funtion for probit regression
#takes a vector y predictor and matrix x as data

probit <- function(b,y, x){

  y <- as.matrix(y)
  x <- as.matrix(x)
  x1 <- cbind(1,x)
  xb <- x1 %*% b

  #compute log liklihood

  return(-sum (y * (log(pnorm(xb))) + (1 -y)* log(1-pnorm(xb))))
}

x <- cbind()
# your data goes here

xint <- cbind(1,x)
#vec 1s for intercept

y <- #data$variable

beta.ols <- (solve(t(xint)%*% xint)) %*% (t(xint)%*%)
#get ols estimates

prob <- optim(bols,llprobit,x=x,y=y,hessian = T)
#get estimates for the probit coeffecients.
#optim() optimizes liklihood

stderrors <- as.matrix(sqrt(diag(solve(out$hessian))))

tscore <- out$par / stderrors

print(cbind(round(out$par,stderrors, tscore)))
#outputs coeffecients stnderrors and a tscore
