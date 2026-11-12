# Nombre: Mateo Cárdenas
# EJERCICIO 1
# Literal a
N <- 100000
# Para la parte positiva
f <- function(x){
  return(x^2*exp(-x^2-exp(x)))
}
# Para hacia menos infinito
f1 <- function(u){
  return(u^2*exp(-u^2-exp(-u)))
}

# Dividimos la integral en su parte de -inf a 0 y de 0 a inf. Usamos la mitad de puntos para uno y la otra mitad para el otro
yp <- runif(N/2)
xp <- (1/yp) - 1
estimador <- f(xp)/yp^2
estimacion <- mean(estimador)

yn <- runif(N/2)
xn <- (1/yn) - 1
estimador1 <- f1(xn)/yn^2
estimacion1 <- mean(estimador1)

integral_total <- estimacion + estimacion1
cat('El valor de la integral es:', integral_total, '\n')
# Calculo de la desviacion ponderando
desviacion <- sqrt(var(estimador)/(N/2) + var(estimador1)/(N/2))

# Literal b
# Como es al 95% usamos de z_alpha a tipico 1.96
alpha <- (1-0.95)
z <- qnorm(1 - alpha/2)
lim_inf <- integral_total - z * desviacion
lim_sup <- integral_total + z * desviacion
cat('Intervalo de confianza al 95% de la integral:', lim_inf, 'a', lim_sup, '\n')



# EJERCICIO 2

# Literal a
mti <- function(probs, labels){ # Generamos N para las sim de S
  U <- runif(1)
  pos <- which(U < cumsum(probs))[1]
  return (labels[pos])
}

deducible <- 300
probs <- c(0.3, 0.25, 0.2, 0.12, 0.08, 0.05)
labels <- c(0L, 1L, 2L, 3L, 4L, 5L)

indemnizacion <- function(N){ # Generamos un único S
  if(N == 0){
    return(0)
  } else {
    accidentes <- rexp(N, rate = 1/1500) # Costo de N accidentes
    total <- sum(max(accidentes - deducible, 0)) # aplicar deducible por cada reclamo y sumar
    return(total)
  }
}

# Literal b
m <- 10000
S <- sapply(1:m, function(k){indemnizacion(mti(probs, labels))}) # Generemos m variables S
esperanza <- mean(S)
varianza <- var(S)

cat('Esperanza de indemnización:', esperanza, '\n')
cat('Varianza de indemnización:', varianza, '\n')

# El precio de poliza es
P <- ((esperanza + 50) / (1 - 0.08)) * 1.15
cat('Precio de la póliza P:', P, '\n')



# EJERCICIO 3
# 100 personas, compras con Poisson(6) por persona, devoluciones con Binomial(n, 0.12)
m <- 10000        
n <- 100     
lambda <- 6   
p <- 0.12 

simular_devoluciones_hora <- function() {
  # usamos la poisson para ver cuanto compran
  compras <- rpois(n, lambda)
  total <- sum(compras)
  # usamos la binomial para ver cuantos devuelven
  total_devoluciones <- rbinom(1, size = total, prob = p)
  return(total_devoluciones)
}

res <- replicate(m, simular_devoluciones_hora())

# Literal b
# IC de total
cat('Promedio de devoluciones:', mean(res), '\n')
int_conf <- quantile(res, c(0.025, 0.975))
cat('Intervalo de confianza (95%) para las devoluciones:', int_conf[1], 'a', int_conf[2], '\n')
# Superar 90 devoluciones
supera_90 <- mean(res > 90)
cat('Probabilidad de que las devoluciones superen 90:', supera_90, '\n')

f <- function(x){
  return(x^2 * exp(-x^2 - exp(x)))
}

x <- runif(1000000)

# Multiplicamos por pi para corregir el factor del cambio de variable
integral_estimada <- pi * mean(f(tan(pi*(x - 0.5))) * (1 / (cos(pi*(x - 0.5))^2)))
print(integral_estimada)
