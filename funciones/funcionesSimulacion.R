

funcionSimulaMas <- function(df, n, M){
  resultado <- matrix(rep(0, length(resultadoGlobal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoGlobal)
  
  for(i in 1:M){
    muestra <- sample(df$seccion_casilla, n)
    df %>%
      filter(seccion_casilla %in% muestra) %>%
      summarise(PRI = sum(total_coalicion) / sum(votacion_total_emitida),
                PAN = sum(pan) / sum(votacion_total_emitida),
                PRD = sum(prd) / sum(votacion_total_emitida)) %>%
      as.numeric() -> resultado[i, ]
  }
  
  # Se convierte a data frame, se multiplica por 100 y se calcula error
  resultado <- data.frame(resultado * 100) #%>%
  #mutate(dif.PRI = PRI - resultadoGlobal$PRI,
  #         dif.PAN = PAN - resultadoGlobal$PAN,
  #         dif.PRD = PRD - resultadoGlobal$PRD)
  return(resultado)
}

funcionSimulaSist <- function(df, n, N, M){
  # Se calcula k, la posición de la casilla inicial se elige al azar entre 1 y k
  k <- round(N/n,0)  # número de estratos
  
  # Crea una matriz vacía
  resultado <- matrix(rep(0, length(resultadoGlobal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoGlobal)
  
  for(i in 1:M){
    # Selección de posición de casilla inicial
    casilla0 <- sample(1:k, 1)
    
    # Seleccion de posición del resto de casillas
    posicion <- c()
    for(j in 1: n-1) {
      posicion <- c(posicion, casilla0 + j*k)
    }
    muestra <- df$seccion_casilla[posicion]
    df %>%
      filter(seccion_casilla %in% muestra) %>%
      summarise(PRI = sum(total_coalicion) / sum(votacion_total_emitida),
                PAN = sum(pan) / sum(votacion_total_emitida),
                PRD = sum(prd) / sum(votacion_total_emitida)) %>%
      as.numeric() -> resultado[i, ]
  }

  # Se convierte a data frame, se multiplica por 100 y se calcula error
  resultado <- data.frame(resultado * 100)
}

funcionSimulaEstrSimple <- function(df, n, M, df.distritos){
  distritos <- df.distritos$distrito_local
  L <- length(distritos)
  
  casillas.xDistrito <- round(n / L)
  
  # Crea una matriz vacía
  resultado <- matrix(rep(0, length(resultadoGlobal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoGlobal)
  
  for(i in 1:M){
    muestra <- c() 
    for(j in 1: L){
      df %>%
        filter(distrito_local == distritos[j]) %>%
        select(seccion_casilla) -> df.muestra
      muestra <- c(muestra, sample(df.muestra$seccion_casilla, casillas.xDistrito))
    } 
    
    # Crea un data frame temporal con estimadores para cada estrato
    df.temporal <- df %>%
      filter(seccion_casilla %in% muestra) %>%
      group_by(distrito_local) %>% 
      summarise(PRI = sum(total_coalicion)/sum(votacion_total_emitida),
                PAN = sum(pan) / sum(votacion_total_emitida),
                PRD = sum(prd) / sum(votacion_total_emitida))
    
    resultado[i, ] <- c(
      estPRI = df.distritos$N.dist %*% df.temporal$PRI / N,
      estPAN = df.distritos$N.dist %*% df.temporal$PAN / N,
      estPRD = df.distritos$N.dist %*% df.temporal$PRD / N)
  }
  rm(df.temporal, df.muestra)
  
  # Se convierte a data frame, se multiplica por 100 y se calcula error
  resultado <- data.frame(resultado * 100) 
  return(resultado)
}

funcionSimulaEstrProp <- function(df, n, M, df.distritos){
  distritos <- df.distritos$distrito_local
  L <- length(distritos)
  
  # Crea una matriz vacía
  resultado <- matrix(rep(0, length(resultadoGlobal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoGlobal)

  for(i in 1:M){
    muestra <- c() 
    for(j in 1: L){
      df %>%
        filter(distrito_local == distritos[j]) %>%
        select(seccion_casilla) -> df.muestra
      muestra <- c(muestra, sample(df.muestra$seccion_casilla, df.distritos$n.dist[j]))
    } 
    
    df.temporal <- df %>%
      filter(seccion_casilla %in% muestra) %>%
      group_by(distrito_local) %>% 
      summarise(PRI = sum(total_coalicion)/sum(votacion_total_emitida),
                PAN = sum(pan) / sum(votacion_total_emitida),
                PRD = sum(prd) / sum(votacion_total_emitida)) 
    
    df.temporal <- df.distritos %>%
      left_join(df.temporal) %>%
      summarise(estPRI = N.dist %*% PRI / N,
                estPAN = N.dist %*% PAN / N,
                estPRD = N.dist %*% PRD / N) %>%
      as.numeric() -> resultado[i, ]
  }

  resultado <- data.frame(resultado * 100)
  return(resultado)  
}


