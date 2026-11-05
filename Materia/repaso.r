dados <- function(){
  res <- sample(1:6,size =2,replace=TRUE)
  return(res)
}
juego <- function(capital){
  num <- 0
  flag <- 1
  capital_T <- c(capital) 
  while(capital>=8 && capital <= 1000){
    res <- dados()
    num <- num + 1

    if (res[1]==res[2]){ # Si tenemos mas de 2 podemos usar max igual a min asi todos serán iguales si coinciden
      capital <- capital + 50
    }else{
      capital <- capital - 8
    }
    capital_T <- c(capital_T,capital)
  }
  if(capital<8){
    # cat('El capital es insuficiente para iniciar el juego')
    flag <- 0
  }
  return(list(Capital = capital_T, Partidas = num, Alerta = flag)) # Parecido a diccionario
}

res_unico <- juego(100)
plot(1:length(res_unico$Capital), res_unico$Capital, type = 's', col = 'blue')

# 2. Ejecuta las 1000 simulaciones (usamos 'replicate' que es más limpio, o 'lapply')
res_mil <- replicate(10000, juego(100), simplify = FALSE)

# 3. Opción corregida manteniendo tu lógica (usando 'j'):
partidas <- sapply(1:10000, function(k) { res_mil[[k]]$Partidas })[
  sapply(1:10000, function(j) { res_mil[[j]]$Alerta }) == 0
]

# 4. Resultados finales
mean(partidas)
length(partidas)


##########################
# Ejercicio 2
##########################
# Inicia: 6am
# Finaliza: 6pm
# Minutos de atención: 12*60 = 720



duracion <- function(ntelefono){
  if(ntelefono == 1){
    return(rpois(1, lambda=2.5))
  } else if(ntelefono == 2){
    return(rpois(1, lambda=3.1))
  } else {
    return(rpois(1, lambda=1.2))
  }
}

tespera <- function(n){
  return(rexp(n, rate = 1/1.5))
}

# Simulamos
sim_telefono <- function(horas){
  # Número de llamadas por minuto
  nllamadas <<- rpois(horas * 60, lambda=2.7)
  espera <- numeric(horas * 60)
  espera[1] <- ifelse(nllamadas[1] > 3, nllamadas[1] - 3, 0)
  
  telefonos <- matrix(0, nrow=horas * 60, ncol=3)
  size_val <- ifelse(nllamadas[1] > 3, 3, nllamadas[1])
  if(size_val > 0) {
    telefonos[1, sample(1:3, size = size_val, replace=FALSE)] <- 1
  }
  
  for(i in 2:(horas * 60)){
    resta_llamadas <- ifelse(espera[i-1]>0,sum(tespera(espera[i-1]) < 1),0)
    espera[i] <- espera[i-1] + nllamadas[i] - resta_llamadas
  }
  return(list(espera,telefonos))
} 

sim_telefono(0.25)

