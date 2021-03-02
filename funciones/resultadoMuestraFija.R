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
    summarise(PAN = sum(pan, na.rm = TRUE),
              PRI = sum(total_coalicion, na.rm = TRUE), 
              PRD= sum(prd, na.rm = TRUE),
              PVEM= sum(pvem, na.rm = TRUE), 
              PT= sum(pt, na.rm = TRUE), 
              MORENA= sum(morena, na.rm = TRUE), 
              HUMANISTA= sum(humanista, na.rm = TRUE), 
              PES= sum(encuentro_social, na.rm = TRUE), 
              noREG = sum(candidato_no_registrado, na.rm = TRUE), 
              NULOS = sum(votos_nulos, na.rm = TRUE), 
              LN = sum(lista_nominal, na.rm = TRUE),
              Total = sum(votacion_total_emitida, na.rm = TRUE))
  df.temporal <- left_join(df.temporal, df.estratos) 
  df.temporal <- df.temporal %>%
    mutate(Wh = vecNh / vecnh)
  
  estTotal <- df.temporal$Wh %*% df.temporal$Total %>% c()
  estPAN <- df.temporal$Wh %*% df.temporal$PAN / estTotal %>% c()
  estPRI <- df.temporal$Wh %*% df.temporal$PRI / estTotal %>% c()
  estPRD <- df.temporal$Wh %*% df.temporal$PRD / estTotal %>% c()
  estPT <- df.temporal$Wh %*% df.temporal$PT / estTotal %>% c()
  estMORENA <- df.temporal$Wh %*% df.temporal$MORENA / estTotal %>% c()
  estHUMANISTA <- df.temporal$Wh %*% df.temporal$HUMANISTA / estTotal %>% c()
  estPES <- df.temporal$Wh %*% df.temporal$PES / estTotal %>% c()
  estnoREG <- df.temporal$Wh %*% df.temporal$noREG / estTotal %>% c()
  estNULOS <- df.temporal$Wh %*% df.temporal$NULOS / estTotal %>% c()
  estLN <- df.temporal$Wh %*% df.temporal$LN %>% c()
  
  deltaPAN <- calculaDelta(df, df.estratos, muestra, df$pan, estPAN, estTotal)
  deltaPRI <- calculaDelta(df, df.estratos, muestra, df$total_coalicion, estPRI, estTotal)
  deltaPRD <- calculaDelta(df, df.estratos, muestra, df$prd, estPRD, estTotal) 
  deltaPT  <- calculaDelta(df, df.estratos, muestra, df$pt, estPT, estTotal) 
  deltaMORENA  <- calculaDelta(df, df.estratos, muestra, df$morena, estMORENA, estTotal) 
  deltaHUMANISTA  <- calculaDelta(df, df.estratos, muestra, df$humanista, estHUMANISTA, estTotal) 
  deltaPES  <- calculaDelta(df, df.estratos, muestra, df$encuentro_social, estPES, estTotal) 
  deltanoREG  <- calculaDelta(df, df.estratos, muestra, df$candidato_no_registrado, estnoREG, estTotal) 
  deltaNULOS  <- calculaDelta(df, df.estratos, muestra, df$votos_nulos, estNULOS, estTotal) 

  resultado <- data.frame(estPRI, estPAN, estPRD, estPT, 
                          estMORENA, estHUMANISTA, estPES, estnoREG,
                          estNULOS, estTotal, estLN,
                          deltaPAN, deltaPRI,deltaPRD,deltaPT,deltaMORENA,deltaHUMANISTA,
                          deltaPES, deltanoREG, deltaNULOS)
  
  resultado <- resultado %>%
    as.data.frame()
  rm()
  return(resultado)
}

