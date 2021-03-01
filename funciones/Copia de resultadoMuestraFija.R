#simEstratificadoMuestraFija(df0, df.distritosTipo, muestra)

calculaDelta <- function(df, df.estratos, muestra, partido, estPART, estTotal){
  df.estratos0 <- df %>%
    group_by(estrato) %>%
    summarise(vecnh = n()) %>%
    left_join(df.estratos) %>%
    select(estrato, vecnh, vecNh)
  
  vecEstratos <- df.estratos0$estrato
  vecnh <- df.estratos0$vecnh
  vecNh <- df.estratos0$vecNh
  vecPART <- data.frame(seccion_casilla=df$seccion_casilla, partido)
  vecPART <- vecPART %>%
    filter(seccion_casilla %in% muestra) %>%
    select(partido)
  #partido <- df$pan
  #estPART <- estPAN
  #estTotal <- estTotal
  df2 <- df %>%
    filter(seccion_casilla %in% muestra) %>%
    mutate(Ghi= (vecPART - estPART*votacion_total_emitida) / estTotal)
  
  #df2 %>%
  #  group_by(estrato) %>%
  #  summarise(Gh.barra = mean(Ghi, na.rm = TRUE)) -> Gh.barra
  
  df2 %>%
    group_by(estrato) %>%
    summarise(suma = sum(Ghi),
              cant = n(),
              Gh.barra = suma/cant) %>%
    select(estrato, Gh.barra) -> Gh.barra
  
  
  df2 <- df2 %>%
    left_join(Gh.barra) %>%
    mutate(Ghi_Gh = (Ghi - Gh.barra)^2) 
  
  df2 %>%
    group_by(estrato) %>%
    summarise(numerador = sum(Ghi_Gh)) %>%
    mutate(denominador = vecnh - 1,
           VarGhi = numerador / denominador) %>%
    select(VarGhi) -> VarGhi
  
  
  varianza <- sum(vecNh^2 * (1/vecnh - 1/vecNh) * VarGhi, na.rm = TRUE)
  
  delta <- sqrt(varianza) * z
  return(delta)
}



resultadoMuestraFija <- function(df, df.estratos, muestra){
  
  #df <- df0  # para pruebas
  #df.estratos <- df.distritosTipo # para pruebas
  #muestra<-muestra0 # para pruebas
  
  df.estratos <- df %>%
    group_by(estrato) %>%
    summarise(vecnh = n()) %>%
    left_join(df.estratos) %>%
    select(estrato, vecnh, vecNh = Nh)
    
  vecEstratos <- df.estratos$estrato
  vecnh <- df.estratos$vecnh
  vecNh <- df.estratos$vecNh
  
  L <- length(vecEstratos)
  
  resultado <- data.frame()
  
  # Se calcula el porcentaje de votos de cada estrato para cada muestra
  df.temporal <- df %>%
    filter(seccion_casilla %in% muestra) %>%
    group_by(estrato) %>% 
    summarise(PRI = sum(total_coalicion, na.rm = TRUE), 
              PAN = sum(pan, na.rm = TRUE),
              Total = sum(votacion_total_emitida, na.rm = TRUE))
  df.temporal <- left_join(df.temporal, df.estratos) 
  df.temporal <- df.temporal %>%
    mutate(Wh = vecNh / vecnh)
  
  estTotal = df.temporal$Wh %*% df.temporal$Total %>% c()
  estPAN = df.temporal$Wh %*% df.temporal$PAN / estTotal %>% c()
  estPRI = df.temporal$Wh %*% df.temporal$PRI / estTotal %>% c()
  
  prueba <- calculaDelta(df, df.estratos, muestra, df$pan, estPAN, estTotal)
  df2 <- df %>%
    filter(seccion_casilla %in% muestra) 
  
  df2 <- df2 %>%
    mutate(GhiPAN = (pan - estPAN*votacion_total_emitida) / estTotal,
           GhiPRI = 
             (total_coalicion - estPRI*votacion_total_emitida) / estTotal)
  df2 %>%
    group_by(estrato) %>%
    summarise(Gh.barraPAN = mean(GhiPAN, na.rm = TRUE)) -> Gh.barraPAN
  
  df2 %>%
    group_by(estrato) %>%
    summarise(Gh.barraPRI = mean(GhiPRI, na.rm = TRUE)) -> Gh.barraPRI
  
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
  
  varPAN <- sum(vecNh^2 * (1/vecnh - 1/vecNh) * VarGhiPAN, na.rm = TRUE)
  varPRI <- sum(vecNh^2 * (1/vecnh - 1/vecNh) * VarGhiPRI, na.rm = TRUE)
  
  deltaPAN <- sqrt(varPAN) * z
  deltaPRI <- sqrt(varPRI) * z
  
  resultado <- cbind(estPRI, estPAN, 
                     #varPRI, varPAN, 
                     deltaPAN, deltaPRI, prueba)
  #colnames(resultado) <- c("estPRI", "estPAN", 
                           #"varPRI", "varPAN", 
                           #"deltaPAN", "deltaPRI")
  
  resultado <- resultado %>%
    as.data.frame()
  rm()
  return(resultado)
}

