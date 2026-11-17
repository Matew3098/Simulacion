# Sistema de linea de espera con un servidor
# Distribucion para las llegadas: exponencial con parametro 1/5
# Distribucion para tiempos de servicio: exponencial con parametro 1/5
# Funcion de tiempos de llegada
Tllegada <- function(){
  return(rexp(1,rate=1/5))
}

sles1s <- function(){
  t <- NNA <- NND <- 0
  ES <- 0
  # Generamos el primer tiempo de llegada
  T0 <- Tllegada()
  TA <- T0
  TD <- Inf

}