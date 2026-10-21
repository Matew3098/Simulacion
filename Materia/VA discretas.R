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