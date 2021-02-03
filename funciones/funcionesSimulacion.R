
varEstMAS <- function(x, n, N){
  y <- (1-n/N) * sd(x)^2/n
  return(y)
}

funcionSimulaMas <- function(df, n, M){
  resultado <- matrix(rep(0, length(resultadoReal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoReal)
  #colnames(resultado) <- c(colnames(resultadoReal), paste("var", colnames(resultadoReal), sep = ""))
  N<-length(df$seccion_casilla)
  
  for(i in 1:M){
    muestra <- sample(df$seccion_casilla, n)
    df %>%
      filter(seccion_casilla %in% muestra) %>%
      summarise(PRI = 100*sum(total_coalicion) / sum(votacion_total_emitida),
                PAN = 100*sum(pan) / sum(votacion_total_emitida)) %>%
      as.numeric() -> resultado[i, ]
    resultado
  }
  
  resultado <- data.frame(resultado) %>%
    mutate(dif.PRI = PRI - resultadoReal$PRI,
           dif.PAN = PAN - resultadoReal$PAN)
  return(resultado)
}

funcionSimulaSist <- function(df, n, N, M){
  # Se calcula k, la posición de la casilla inicial se elige al azar entre 1 y k
  k <- round(N/n,0)  # número de estratos
  
  # Crea una matriz vacía
  resultado <- matrix(rep(0, length(resultadoReal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoReal)
  
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
      summarise(PRI = 100*sum(total_coalicion) / sum(votacion_total_emitida),
                PAN = 100*sum(pan) / sum(votacion_total_emitida)) %>%
      as.numeric() -> resultado[i, ]
  }

  # Se convierte a data frame, se multiplica por 100 y se calcula error
  resultado <- data.frame(resultado) %>%
    mutate(dif.PRI = PRI - resultadoReal$PRI,
           dif.PAN = PAN - resultadoReal$PAN)
}

funcionSimulaEstrSimple <- function(df, n, M, df.distritos){
  distritos <- df.distritos$distrito_local
  L <- length(distritos)
  
  casillas.xDistrito <- round(n / L)
  
  # Crea una matriz vacía
  resultado <- matrix(rep(0, length(resultadoReal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoReal)
  
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
      summarise(PRI = 100*sum(total_coalicion)/sum(votacion_total_emitida),
                PAN = 100*sum(pan) / sum(votacion_total_emitida))
    
    resultado[i, ] <- c(
      estPRI = df.distritos$N.dist %*% df.temporal$PRI / N,
      estPAN = df.distritos$N.dist %*% df.temporal$PAN / N)
  }

  # Se convierte a data frame, se multiplica por 100 y se calcula error
  resultado <- data.frame(resultado)  %>%
    mutate(dif.PRI = PRI - resultadoReal$PRI,
           dif.PAN = PAN - resultadoReal$PAN)
  return(resultado)
}

funcionSimulaEstrProp <- function(df, n, M, df.distritos){
  distritos <- df.distritos$distrito_local
  L <- length(distritos)
  
  # Crea una matriz vacía
  resultado <- matrix(rep(0, length(resultadoReal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoReal)

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
      summarise(PRI = 100*sum(total_coalicion)/sum(votacion_total_emitida),
                PAN = 100*sum(pan) / sum(votacion_total_emitida)) 
    
    df.temporal <- df.distritos %>%
      left_join(df.temporal) %>%
      summarise(estPRI = N.dist %*% PRI / N, #PRI es el vector p^
                estPAN = N.dist %*% PAN / N) %>% #,
                #varPRI = (1-n.dist/N.dist)) %>%
      as.numeric() -> resultado[i, ]
  }

  resultado <- data.frame(resultado)  %>%
    mutate(dif.PRI = PRI - resultadoReal$PRI,
           dif.PAN = PAN - resultadoReal$PAN)
  return(resultado)  
}

funcionSimulaEstr02 <- function(df, n, M, df.distritos){
  distritos <- df.distritos$distrito_tipo
  L <- length(distritos)
  # Crea una matriz vacía
  resultado <- matrix(rep(0, length(resultadoReal)*M), nrow = M)
  colnames(resultado) <- colnames(resultadoReal)
  
  for(i in 1:M){
    # selección de una muestra
    muestra <- c() 
    for(j in 1: L){
      gobernador2015 %>%
        filter(distrito_tipo == distritos[j]) %>%
        select(seccion_casilla) -> df.muestra
      muestra <- c(muestra, sample(df.muestra$seccion_casilla, df.distritos$n.dist[j]))
    } 
    
    # Crea un data frame temporal con estimadores para cada estrato
    df.temporal <- gobernador2015 %>%
      filter(seccion_casilla %in% muestra) %>%
      group_by(distrito_tipo) %>% 
      summarise(PRI = 100*sum(total_coalicion)/sum(votacion_total_emitida),
                PAN = 100*sum(pan) / sum(votacion_total_emitida)) 
    
    # Agrega número de casillas
    df.temporal <- df.distritos %>%
      left_join(df.temporal) %>%
      summarise(estPRI = N.dist %*% PRI / N,
                estPAN = N.dist %*% PAN / N) %>%
      as.numeric() -> resultado[i, ]
  }
  # Se convierte a data frame, se multiplica por 100 y se calcula error
  resultado <- data.frame(resultado) %>%
    mutate(dif.PRI = PRI - resultadoReal$PRI,
           dif.PAN = PAN - resultadoReal$PAN)
}
