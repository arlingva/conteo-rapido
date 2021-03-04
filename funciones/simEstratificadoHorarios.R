simEstratificadoHorarios <- function(df, vecEstratos, vecNh, vecnh, pcapturado){
  L <- length(vecEstratos)
  
  resultado <- data.frame()
  estPRI <-rep(0, M)
  estPAN <-rep(0, M)
  varPRI <-rep(0, M)
  varPAN <-rep(0, M)
  estTotal <-rep(0, M)
  #i<-1
  for(i in 1:M){
    muestra <- c() 
    # Selecciona la muestra dentro de cada estrato
    for(j in 1: L){
      df %>%
        filter(estrato == vecEstratos[j]) %>%
        select(seccion_casilla) -> df.muestra
      muestra <- c(muestra, sample(df.muestra$seccion_casilla, vecnh[j]))
    }
    
    # Se calcula el porcentaje de votos de cada estrato para cada muestra
    df.temporal <- df %>%
      filter(seccion_casilla %in% muestra) %>%
      group_by(estrato) %>% 
      summarise(PRI = sum(total_coalicion), #suma sobre las casillas: 1 hasta vecnh
                PAN = sum(pan),   # sum (Yhi)
                Total = sum(votacion_total_emitida)) # Xh
    df.temporal <- cbind(df.temporal, Wh = vecNh / vecnh)
    
    estTotal[i] = df.temporal$Wh %*% df.temporal$Total #X gorro
    estPAN[i] = df.temporal$Wh %*% df.temporal$PAN / estTotal[i]
    estPRI[i] = df.temporal$Wh %*% df.temporal$PRI / estTotal[i]
    
    df2 <- df %>%
      filter(seccion_casilla %in% muestra) %>%
      select(estrato, seccion_casilla, 
             total_coalicion, pan, votacion_total_emitida) %>%
      mutate(GhiPAN = (pan - estPAN[i]*votacion_total_emitida) / estTotal[i],
             GhiPRI = 
               (total_coalicion - estPRI[i]*votacion_total_emitida) / estTotal[i])
    
    df2 %>%
      group_by(estrato) %>%
      summarise(Gh.barraPAN = mean(GhiPAN)) -> Gh.barraPAN
    
    df2 %>%
      group_by(estrato) %>%
      summarise(Gh.barraPRI = mean(GhiPRI)) -> Gh.barraPRI
    
    df2 <- df2 %>%
      left_join(Gh.barraPAN) %>%
      mutate(Ghi_GhPAN = (GhiPAN - Gh.barraPAN)^2) %>%
      left_join(Gh.barraPRI) %>%
      mutate(Ghi_GhPRI = (GhiPRI - Gh.barraPRI)^2)
    
    df2 %>%
      group_by(estrato) %>%
      summarise(numeradorPAN = sum(Ghi_GhPAN)) %>%
      mutate(denominador = vecnh - 1,
             VarGhiPAN = numeradorPAN / denominador) %>%
      select(VarGhiPAN) -> VarGhiPAN
    
    df2 %>%
      group_by(estrato) %>%
      summarise(numeradorPRI = sum(Ghi_GhPRI)) %>%
      mutate(denominador = vecnh - 1,
             VarGhiPRI = numeradorPRI / denominador) %>%
      select(VarGhiPRI) -> VarGhiPRI
    
    varPAN[i] <- sum(vecNh^2 * (1/vecnh - 1/vecNh) * VarGhiPAN)
    varPRI[i] <- sum(vecNh^2 * (1/vecnh - 1/vecNh) * VarGhiPRI)
  }    
  
  deltaPAN <- sqrt(varPAN) * 2.575
  deltaPRI <- sqrt(varPRI) * 2.575
  
  resultado <- cbind(estPRI, estPAN, varPRI, varPAN, deltaPAN, deltaPRI)
  colnames(resultado) <- c("estPRI", "estPAN", 
                           "varPRI", "varPAN", 
                           "deltaPAN", "deltaPRI")
  
  resultado <- resultado %>%
    as.data.frame() %>%
    mutate(dif.PRI = estPRI - resultadoReal$PRI,
           dif.PAN = estPAN - resultadoReal$PAN)
  return(resultado)
}

# Valores para probar función
#df <- gobernador2015  %>% rename(estrato = distrito_tipo)
#vecEstratos <- df.distritosTipo$estrato
#vecNh <- df.distritosTipo$Nh
#vecnh <- df.distritosTipo$nh


resumenSimulacion <- function(df, vz){
  df %>%
    mutate(MinPRI = estPRI - (vz * sqrt(varPRI)),
           MaxPRI = estPRI + (vz * sqrt(varPRI)),
           MinPAN = estPAN - (vz * sqrt(varPAN)),
           MaxPAN = estPAN + (vz * sqrt(varPAN)),
           capturaPRI = resultadoReal$PRI > MinPRI & resultadoReal$PRI < MaxPRI,
           capturaPAN = resultadoReal$PAN > MinPAN & resultadoReal$PAN < MaxPAN,
           #longIntPRI = MaxPRI-MinPRI,
           #longIntPAN = MaxPAN-MinPAN,
           no.Traslape = MaxPAN < MinPRI | MaxPRI < MinPAN
    )
}

resumenCaptura <- function(df){
  rbind(df %>%
          select(capturaPRI, capturaPAN, no.Traslape) %>%
          summarise(PRI.capturado = 100*mean(capturaPRI),
                    PAN.capturado = 100*mean(capturaPAN),
                    no.Traslapado = 100*mean(no.Traslape)))
}

# Valores para probar función
#df <- gobernador2015  %>% rename(estrato = distrito_local)
#vecEstratos <- df.distritosTipo$estrato
#vecNh <- df.distritosTipo$Nh
#vecnh <- df.distritosTipo$nh



#hora=rand.date("2021-06-06 18:00:00","2021-06-07 08:00:00", 2)
