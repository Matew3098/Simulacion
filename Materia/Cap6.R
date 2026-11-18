# Sistema de linea de espera con un servidor
# Distribucion para las llegadas: exponencial con parametro 1/5
# Distribucion para tiempos de servicio: exponencial con parametro 1/5
# Funcion de tiempos de llegada
Tllegada <- function(){
  return(rexp(1,rate=1/5))
}

# Funcion de tiempos de salida
Tservicio <- function(){ # Es el y
  return(rexp(1,rate=1/5))
}

sles1s <- function(TT){
  t <- NNA <- NND <- 0
  ES <- 0
  ESS <- c(ES)
  eventos <- c(0)
  # Generamos el primer tiempo de llegada
  T0 <- Tllegada()
  TA <- T0
  TD <- Inf
  A <- D <- 0

  while(t < TT || ES > 0){
    # Caso 1
    if(TA <= TD & TA <= TT){
      t <- TA
      NNA <- NNA + 1
      ES <- ES + 1 
      TA <- t + Tllegada()
      if(ES == 1){
        TD <- t + Tservicio()
      }
      A [NNA] <- t
      ESS <- c(ESS,ES)
      eventos <- c(eventos,t)
    }
    # Caso 2
    if(TD < TA & TD <= TT){
      t <- TD
      ES <- ES - 1
      NND <- NND + 1
      if(ES == 0){
        TD <- Inf
      } else {
        TD <- t + Tservicio()
      }
      D[NND] <- t
      ESS <- c(ESS,ES)
      eventos <- c(eventos,t)
    }
    # Caso 3
    if(min(TA,TD) > TT & ES > 0){
      t <- TD
      ES <- ES - 1
      NND <- NND + 1
      if(ES > 0){
        TD <- t + Tservicio()
      }
      D[NND] <- t
      ESS <- c(ESS,ES)
      eventos <- c(eventos,t)
    }
    if(min(TA,TD) > TT & ES == 0){
      Tp <- max(t-TT,0)
    }
    
  }
  return(list(TiemposArribo=A, TiemposSalida=D,TiempoExtra=Tp,Espera=ESS,Eventos=eventos))
}
set.seed(12345)
res <- sles1s(TT=50)
espera <- res$Espera
eventos <- res$Eventos
plot(eventos,espera,type='s',col='red')
abline(v=50)
