# Procesos de Poisson homogengeos
pph <- function(T, lambda){
  t <- 0
  I <- 0
  S <- c()
  while(t < T){
    I <- I + 1
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
  return(10 + s)
}

x <- seq(0, 1, by =0.01)
plot(x,flambda(x),type='l', col='red',ylim=c(0,11))
abline(v=0.5, lty = 2)

lambda_j <- function(t){
  if(t<=0.5){
    return(10.5)
  }else{
    return(11)
  }
}

ppnh <- function(T, n){ # n es el numero de particiones para el tiempo
ti <- seq(0, T, length.out = (n+1))[-1] # Genera la particion y elimino el primer elemento que es 0.
lambda_i <- flambda(ti)
t <- I <- 0
J <- 1
S <- c()
X <- (-1/lambda_i[J])*log(runif(1))
while(J <= n){
if((t + X > ti[J]) && (J == n)){
break
} else if((t + X > ti[J]) && (J < (n+1))){
X <- (X - ti[J] + t)*lambda_i[J]/lambda_i[J+1] # En el libro esta mal
t <- ti[J]
J <- J + 1
} else if(t + X <= ti[J]){
t <- t + X
if (runif(1) <= flambda(t)/lambda_i[J]){
I <- I + 1
S[I] <- t
}
X <- (-1/lambda_i[J])*log(runif(1))
}
}
return(S)
}

res <- ppnh(1,8)
plot(res,1:length(res),type='s')
