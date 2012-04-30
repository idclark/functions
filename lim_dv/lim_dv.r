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


######ordered probit
lloprobit <- function(b,x,y){
  x <- as.matrix(x)
  xb <- x %*% b[1:ncol(x)]
  g1 <- as.numeric(b[ncol(x)+1])
  g2 <- as.numeric(b[ncol(x)+2])
  y <- as.matrix(y)

    p1 <- log(pnorm(g1-xb))
    p2 <- log(pnorm(g2-xb)-pnorm(g1-xb))
    p3 <- log(pnorm(xb-g2)) 
  
    z <- rep(1,nrow(x))
    z1 <- as.numeric(z == y)
    z2 <- as.numeric(2*z == y)
    z3 <- as.numeric(3*z == y)
    llik <- sum(z1 * p1 + z2 * p2 + z3 * p3)
  
  return(-llik) 
}

# compute OLS estimates get starting values
bols <- (solve(t(xint) %*% xint)) %*% (t(xint) %*% y)
startval <- rbind(as.matrix(bols[2:nrow(bols)]),0,1)

# estimate the ordered probit model 
out <- optim(startval,lloprobit,x=x,y=y,hessian = TRUE,method="BFGS",control=list(trace=1,REPORT=1))
stderrs <- as.matrix(sqrt(diag(solve(out$hessian))))
print(cbind(out$par,stderrs),digits=3)


lltobit <- function(b,x,y) {
    sigma <- b[3] 
    y  <- as.matrix(y)
    x <- as.matrix(x)
    x <- cbind(1,x)
    bx <- x %*% b[1:2] 
    d <- y != 0 
    llik <- sum(d * ((-1/2)*(log(2*pi) + log(sigma^2) + ((y - bx)/sigma)^2)) + (1-d) * (log(1 - pnorm(bx/sigma))))    
    return(-llik)
}

lltrunc <- function(b,x,y) {
    sigma <- b[3] 
    y  <- as.matrix(y)
    x <- as.matrix(x)
    x <- cbind(1,x)
    bx <- x %*% as.matrix(b[1:2])
    llik <- sum(-1/2*(log(2*pi) + log(sigma^2) + ((y - bx)/sigma)^2) - log(1 - pnorm(-bx/sigma)))
    return(-llik)
}

n.sims <- 500

b.tobit <- rep(NA,n.sims)
b.trunc <- rep(NA,n.sims)
b.probit <- rep(NA,n.sims)

se.tobit <- rep(NA,n.sims)
se.trunc <- rep(NA,n.sims)
se.probit <- rep(NA,n.sims)

limit.obs <- rep(NA,n.sims)

for (s in 1:n.sims){

# generate the data

n <- 500
b <- c(.5,.1)
x <- rnorm(n)
u <- rnorm(n,sd=2)  # use sd=1 or sd=2
ystar <- cbind(1,x) %*% b + u
y <- as.numeric(ystar > 0)*ystar

xint <-  cbind(1,x)

bols <- (solve(t(xint) %*% xint)) %*% (t(xint) %*% y)

startval <- rbind(as.matrix(bols[1:nrow(bols)]),1)

# estimate the tobit model 

out.tobit <- optim(startval,lltobit,x=x,y=y,hessian = TRUE,method="L-BFGS-B",control=list(trace=1,REPORT=1))

ysub <- y[y>0]
xsub <- x[y>0]

out.trunc <- optim(startval,lltrunc,x=xsub,y=ysub,hessian = TRUE,method="L-BFGS-B",control=list(trace=1,REPORT=1))

# estimate probit model

y.dum <- as.numeric(y > 0)

out.probit <- glm(formula=y.dum ~ x,family=binomial(link="probit"))

b.tobit[s] <- out.tobit$par[2]
b.trunc[s] <- out.trunc$par[2]
b.probit[s] <- out.probit$coef[2]

se.tobit[s] <- sqrt(diag(solve(out.tobit$hessian)))[2]
se.trunc[s] <- sqrt(diag(solve(out.trunc$hessian)))[2]
se.probit[s] <- sqrt(diag(vcov(out.probit)))[2]

limit.obs[s] <- length(ysub)

}

print(c("Average number of limit observations:",mean(limit.obs)))

mean.tobit <- mean(b.tobit)
mean.trunc <- mean(b.trunc)
mean.probit <- mean(b.probit)

print(round(c(mean.tobit,mean.trunc,mean.probit),digits=2))
      
mse.tobit <- sum((b[2] - b.tobit)^2)/n.sims
mse.trunc <- sum((b[2] - b.trunc)^2)/n.sims
mse.probit <- sum((b[2] - b.probit)^2)/n.sims

print(round(c(mse.tobit,mse.trunc,mse.probit),digits=3))

optimism.tobit <- 100*(sum((b.tobit-mean(b.tobit))^2) / sum(se.tobit^2))
optimism.trunc <- 100*(sum((b.trunc-mean(b.trunc))^2) / sum(se.trunc^2))
optimism.probit <- 100*(sum((b.probit-mean(b.probit))^2) / sum(se.probit^2))

print(round(c(optimism.tobit,optimism.trunc,optimism.probit),digits=3))

