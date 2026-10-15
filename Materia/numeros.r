#Generación de numeros pseudoaleatorios 
mcm <- function(a, m, x0, n){
  if(is.integer(a) & is.integer(m) & a > 0 & m> 0){
    res <- numeric(n+1)
    res[1] <- x0
    for(i in 2:(n+1)){
      res[i] <- ((a*res[i-1]) %% m)
    }
    return(res[-1]/m)
  } else {
    cat('Los valores de a y m no cumplen las condiciones (enteros positivos)')
  }
}

res <- mcm(a = 31L, m = 67L, x0= 12, n = 1000)
which(res == res[1])
hist(res,breaks = seq(0,1,by=0.05))

valores<-expand.grid(a=c(7L, 19L, 31L, 47L, 71L, 89L), m =c(67L, 321L, 577L, 800L), x0 = c(7,12,22,53))

valores$ciclo <- sapply(1:nrow(valores), function(k){
  res <- mcm(a= valores[k,1], m= valores[k,2], x0= valores[k,3], n=1000)
  which(res == res[1])[2] - 1
})
print(valores)
which.max(valores$ciclo)
which.min(valores$ciclo)


gcm <- function(a, m, c, x0, n){
  if(is.integer(a) & is.integer(m) & a > 0 & m> 0){
    res <- numeric(n+1)
    res[1] <- x0
    for(i in 2:(n+1)){
      res[i] <- ((a*res[i-1] + c) %% m) /m
    }
    return(res[-1])
  } else {
    cat('Los valores de a y m no cumplen las condiciones (enteros positivos)')
  }
}
res1<-gcm(a = 31L, m = 67L,c=0, x0= 12, n = 10)
res2<-gcm(a = 31L, m = 67L,c=1, x0= 12, n = 10)
which(res1[1]==res2)
which(res1[2]==res2)


# Aproximacion de integrales
#f(x) =exp(-x^2)
aprox_fx <- function(k){
  U = runif(k)
  return (mean(exp(-U^2)))
}
#valor teorico 0.74682
res <- sapply(seq(10,1000,by=10),function(j){
  aprox_fx(j)
})
plot(seq(10,1000,by=10),res, type='l')
abline(h=0.74682, col='red')