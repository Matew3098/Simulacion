# Metodo de aceptacion y rechazo caso continuo
fx <- function(x){
  return(20*x*(1-x)^3)
}
plot(seq(0,1,by=0.01),fx(seq(0,1,by=0.01)),type='l', col ='red',
ylab='f(x)',xlab='x')
abline(h=1, col='blue')

gx <- function(x){
  return(1)
}

# Cálculo de la constante c
optimize(f=function(x){fx(x)/gx(x)},interval=c(0,1),maximum = TRUE)

fx(0.25)

# Implementacion

dc <- function(){
  Y <- runif(1) # de la densidad g, podría ser exp, etc la que sea util
  U <- runif(1)
  c <- optimize(f=function(x){fx(x)/gx(x)},interval=c(0,1),maximum = TRUE)$objective
  if (U <=fx(Y)/(c*gx(Y))){
    return (Y)
  } else{
    return (NA)
  }
}

res <-replicate(1000,dc())
sum(is.na(res)) # Valores fallidos al generar la va
sum(!is.na(res)) # Valores validos de la distribución

# Aceptación y rechazo para ejercicio 5e

fx <- function(x){
  return((2/sqrt(pi)) * x^(1/2) * exp(-x))
}

x <- seq(0, 10, by = 0.1)
plot(x, fx(x), type = 'l')

# Función envolvente
g1 <- function(x){
  return(exp(-x))
}

# Hallar la constante
c1 <- optimize(f = function(x){ fx(x) / g1(x) }, interval = c(0, 30), maximum = TRUE)$objective

plot(x, fx(x), type = 'l', ylim = c(0, 0.5))
par(new = TRUE)
plot(x, c1 * g1(x), type = 'l', col = 'red', ylim = c(0, 1))

gamma_sim1 <- function(){
  # Generamos Y (corregido a -log para obtener valores positivos en el dominio [0, Inf))
  Y <- -log(runif(1))
  # Verificación
  if(runif(1) <= fx(Y) / (c1 * g1(Y))){
    return(Y)
  } else {
    return(NA)
  }
}

res <- replicate(1000, gamma_sim1())
length(res[is.na(res)])
length(res[!is.na(res)])


# Función envolvente
g2 <- function(x){
  return(0.5*exp(-0.5*x))
}

# Hallar la constante
c1 <- optimize(f = function(x){ fx(x) / g1(x) }, interval = c(0, 30), maximum = TRUE)$objective

c2 <- optimize(f = function(x){ fx(x) / g2(x) }, interval = c(0, 30), maximum = TRUE)$objective


plot(x, fx(x), type = 'l', ylim = c(0, 0.5))
par(new = TRUE)
plot(x, c1 * g1(x), type = 'l', col = 'red', ylim = c(0, 1))
par(new = TRUE)
plot(x, c2 * g2(x), type = 'l', col = 'blue', ylim = c(0, 1))

gamma_sim2 <- function(){
  # Generamos Y (corregido a -log para obtener valores positivos en el dominio [0, Inf))
  Y <- -2*log(runif(1))
  # Verificación
  if(runif(1) <= fx(Y) / (c2 * g2(Y))){
    return(Y)
  } else {
    return(NA)
  }
}

res <- replicate(10000, gamma_sim2())
length(res[is.na(res)])
length(res[!is.na(res)])



g3 <- function(x, lambda){
  return(lambda * exp(-lambda * x))
}

# 1. Función que calcula la constante c para un lambda dado
# (Maximiza f(x)/g(x) respecto a x)
calcular_c <- function(lambda) {
  opt_x <- optimize(f = function(x) { fx(x) / g3(x, lambda) }, 
                    interval = c(0.001, 30), 
                    maximum = TRUE)
  return(opt_x$objective) # Devuelve el valor máximo (la constante c)
}

# 2. Encontrar el lambda que MINIMIZA esa constante c 
# (optimize por defecto hace minimización si no pones maximum = TRUE)
opt_lambda <- optimize(f = calcular_c, interval = c(0.1, 5))$minimum

# 3. Obtener el c óptimo (c3) y el lambda óptimo
lambda_optimo <- opt_lambda
c3 <- calcular_c(lambda_optimo)

gamma_sim3 <- function() {
  # 1. Generar Y desde la exponencial con lambda_optimo (transformada inversa)
  U1 <- runif(1)
  Y <- -log(U1) / lambda_optimo
  
  # 2. Verificación de aceptación/rechazo
  U2 <- runif(1)
  if (U2 <= fx(Y) / (c3 * g3(Y, lambda_optimo))) {
    return(Y)
  } else {
    return(NA)
  }
}

cat("Lambda óptimo:", lambda_optimo, "\n")
cat("Constante c3 óptima:", c3, "\n")

# Probamos la simulación con 1000 iteraciones
res3 <- replicate(1000, gamma_sim3())

# Cuántos se aceptaron vs rechazaron
cat("Aceptados:", length(res3[!is.na(res3)]), "\n")
cat("Rechazados:", length(res3[is.na(res3)]), "\n")

# Ejemplo 5f 
# Simulacion de Variables normales
normal_sim <- function(){
  Y1 <- -log(runif(1))
  Y2 <- -log(runif(1))
  aux <- Y2 - (Y1-1)^2/2
  if(aux > 0){
    U <- runif(1)
    if(U<=0.5){
      return(Y1)
    } else{
      return(-Y1)
    }
  }else{
    return(NA)
  }
}
res <- replicate(100000,normal_sim())
length(res[is.na(res)])
length(res[!is.na(res)])

mean(res[!is.na(res)])
sd(res[!is.na(res)])

# Metodo polar normales

normal_polar <- function(){
  V1 <- 2*runif(1)-1
  V2 <- 2*runif(1)-1
  S <- V1^2 +V2^2
  if(S<=1){
    X <- sqrt((-2*log(S))/S)*V1
    Y <- sqrt((-2*log(S))/S)*V2
    return(c(NA,NA))
  } else {
    return(c(NA,NA))
  }
}
res <- replicate(10000,normal_polar())
