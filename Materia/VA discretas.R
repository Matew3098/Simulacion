# Generacion de variables aleatorias discretas

# Inputs:
# probs: vector de probabilidades
# labels: etiquetas o valores que puede tomar la variable aleatoria

mti <- function(probs, labels){
  U <- runif(1)
  pos <- which(U < cumsum(probs))[1]
  return (labels[pos])
}

mti_m <- function(probs, labels){
  U <- runif(1)
  pos <- which(U < cumsum(probs))[1]
  return (labels[pos])
}

n <- 3000000

probabilidades <- runif(20); probabilidades <- probabilidades/sum(probabilidades)
labels <- paste0(LETTERS[1:20], '001')
system.time(table(sapply(1:n, function(k){
  mti(probabilidades,labels)}))/n)

# order(vector) me entrega las posiciones que deberian ocupar los elementos para estar ordenados de forma creciente
# para ordenar de forma decreciente decreasing = TRUE
res <- order(probabilidades, decreasing = TRUE)
probabilidades <- probabilidades[res]
labels <- labels[res]

system.time(table(sapply(1:n, function(k){
  mti(probabilidades,labels)}))/n)


# Distribucion Geometrica
# Inputs: 
# q: probabilidad de fracaso

dg <- function(q) {
  return(floor(log(runif(1)) / log(q)) + 1)
}

n1 <- 1000000

res <- sapply(1:n1, function(k){
  dg(q=0.37)
})

table(res) / n1
hist(res)

# 3. Simulación de variables geométricas con R nativo
val_teoricos <- rgeom(n = n1, prob = 1 - 0.37) + 1

# 4. Test de Kolmogorov-Smirnov
ks_resultado <- ks.test(res, val_teoricos)
print(ks_resultado)

# Generacion de variable Poisson
# Input: lambda

dp <- function(lambda){
  U <- runif(1)
  i <- 0
  p <- exp(-lambda)
  Fx <- p
  while(U>= Fx){
    p <- lambda*p/(i+1)
    Fx <- Fx + p
    i <- i + 1
  }
  return(i)
}

res <- replicate(1000000,dp(lambda=4))
mean(res)
var(res)
hist(res, 
     main = "Histograma de la distribución de Poisson (lambda = 4)", 
     xlab = "Valores (i)", 
     ylab = "Frecuencia", 
     col = "lightblue", 
     border = "white")

# Distribucion binomial
# Input n, p

db <- function(n, p) {
  U <- runif(1)
  c <- p / (1 - p)
  pr <- (1 - p)^n
  i <- 0
  Fx <- pr

  while (U > Fx && i < n) {
    pr <- (c * (n - i) / (i + 1)) * pr
    Fx <- Fx + pr
    i <- i + 1
  }

  return(i)
}

res <- replicate(1000000, db(n = 10, p = 0.04))
mean(res)
var(res)
hist(
  res,
  breaks = seq(-0.5, 100.5, by = 1),
  main = "Histograma de la distribución binomial",
  xlab = "Valores de X",
  ylab = "Frecuencia",
  col = "#40E0D0",
  border = "white"
)


# Metodo de aceptación y rechazo

# Variable aleatoria X
# X in {1,2,...,10}
# pj in {0.11, 0.12, 0.09,0.08,0.12,0.10,0.09,0.09,0.10,0.10}

plot(1:10,c(0.11, 0.12, 0.09,0.08,0.12,0.10,0.09,0.09,0.10,0.10),type='s',ylim=c(0,0.15))
abline(h=0.10,col='red')

mar <- function(P,X){
  n <- length(P)
  Y <- 1:n
  qY <- rep(1/n, n) # Distribucion discreta uniforme para generar una variable aleatoria distinta
  cc <<- max(P/qY)
  # condicional
  val_Y <- floor(n*runif(1)) + 1
  while(runif(1) > P[val_Y]/(cc*qY[val_Y])){
    val_Y <- floor(n*runif(1)) + 1
  }
  return(X[val_Y])
}

P <- c(0.17, 0.51, 0.05,0.27)
X <- c(0L, 3L, 7L, 11L)
m <- 10000
res <- replicate(m, mar(P,X))

table(res)/m

# Aplicacion: Serie Logaritmica de Fisher
p <- 0.95
res <- sapply(1:300, function(k){-p^k/(k*log(1-p))})
sum(res)
plot(1:300, res, type ='l', col='red')

X <- 1:300

res2 <- replicate(10000,mar(res, X))
hist(res2)
