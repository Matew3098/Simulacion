# Sistema de linea de espera con un servidor
# Distribucion para las llegadas: exponencial con parametro 1/5
# Distribucion para tiempos de servicio: exponencial con parametro 1/5
# Funcion de tiempos de llegada
Tllegada <- function() {
  return(rexp(1, rate = 1 / 5))
}

# Funcion de tiempos de salida
Tservicio <- function() {
  # Es el y
  return(rexp(1, rate = 1 / 5))
}

sles1s <- function(TT) {
  t <- NNA <- NND <- 0
  ES <- 0
  ESS <- c(ES)
  eventos <- c(0)
  T0 <- Tllegada()
  TA <- T0
  TD <- Inf
  A <- D <- 0
  Tp <- 0

  while (TA <= TT || ES > 0) {
    # Caso 1: Arribo dentro del tiempo de exposición TT
    if (TA <= TD && TA <= TT) {
      t <- TA
      NNA <- NNA + 1
      ES <- ES + 1
      TA <- t + Tllegada()
      if (ES == 1) {
        TD <- t + Tservicio()
      }
      A[NNA] <- t
      ESS <- c(ESS, ES)
      eventos <- c(eventos, t)
    }
    # Caso 2: Salida (ocurre antes del próximo arribo y antes de que se cierren llegadas)
    if (TD < TA && TD <= TT) {
      t <- TD
      ES <- ES - 1
      NND <- NND + 1
      if (ES == 0) {
        TD <- Inf
      } else {
        TD <- t + Tservicio()
      }
      D[NND] <- t
      ESS <- c(ESS, ES)
      eventos <- c(eventos, t)
    }
    # Caso 3: Vaciado del sistema una vez superado TT
    if (min(TA, TD) > TT && ES > 0) {
      t <- TD
      ES <- ES - 1
      NND <- NND + 1
      if (ES == 0) {
        TD <- Inf
      } else {
        TD <- t + Tservicio()
      }
      D[NND] <- t
      ESS <- c(ESS, ES)
      eventos <- c(eventos, t)
    }
  }

  Tp <- max(t - TT, 0)

  return(list(
    TiemposArribo = A,
    TiemposSalida = D,
    TiempoExtra = Tp,
    Espera = ESS,
    Eventos = eventos
  ))
}

set.seed(12345)
res <- sles1s(TT = 50)
espera <- res$Espera
eventos <- res$Eventos
plot(eventos, espera, type = 's', col = 'red')
abline(v = 50)


# Sistema de linea de espera con dos servidores en serie
sle2s <- function(TT) {
  t <- NNA <- NND <- 0
  A1 <- A2 <- D <- 0
  ES <- c(0, 0)
  TA <- Tllegada()
  T1 <- T2 <- Inf

  while (TA <= TT || sum(ES) > 0) {
    # Caso 1
    if (TA <= TT && TA == min(TA, T1, T2)) {
      t <- TA
      NNA <- NNA + 1
      ES[1] <- ES[1] + 1
      if (ES[1] == 1) {
        T1 <- t + Tservicio()
      }
      A1[NNA] <- t

      TA <- t + Tllegada()
      if (TA > TT) {
        TA <- Inf
      }
    }

    # Caso 2
    if (T1 == min(TA, T1, T2)) {
      t <- T1
      ES[1] <- ES[1] - 1
      ES[2] <- ES[2] + 1
      if (ES[1] == 0) {
        T1 <- Inf
      } else {
        T1 <- t + Tservicio()
      }
      if (ES[2] == 1) {
        T2 <- t + Tservicio()
      }
      A2[NNA - ES[1]] <- t
    }

    # Caso 3
    if (T2 == min(T1, T2, TA)) {
      t <- T2
      NND <- NND + 1
      ES[2] <- ES[2] - 1
      if (ES[2] == 0) {
        T2 <- Inf
      } else {
        T2 <- t + Tservicio()
      }
      D[NND] <- t
    }
  }

  return(list(ArriboS1 = A1, ArriboS2 = A2, Salidas = D))
}
