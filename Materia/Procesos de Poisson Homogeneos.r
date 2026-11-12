# Procesos de Poisson homogengeos
pph <- function(T, lambda){
  t <- 0
  I <- 0
  S <- c()
  while(t < T){
    I <- I +1
    S[I] <- t
    t <- t + (-1/lambda)*log(runif(1))
  }
  return(S)
}
res <- pph(10,lambda=3)
plot(0:(length(res)-1), res, type='s')

pph2 <- function(T, lambda){
  N <- rpois(1, lambda = T*lambda)
  t <- sort(N*runif(N))
  return(t)
}

res1 <- pph2(10,lambda=3)
plot(0:(length(res1)-1), res1, type='s')


# Procesos de Poisson no homogengeos
flambda <- function(s){
  return(10+s)
}

x <- seq(0, 1, by =0.01)
plot(x,flambda(x),type='l', col='red',ylim=c(0,11))
abline(h=0.5)
abline(v=0.5)

lambda_j <- function(t){
  if(t<=0.5){
    return(10.5)
  }else{
    return(11)
  }
}

