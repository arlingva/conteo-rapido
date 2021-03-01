resultadoOtrosPartidos <- function(df, df.estratos, muestra){
  
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
              PRD = sum(prd, na.rm = TRUE), 
              PVEM = sum(pvem, na.rm = TRUE), 
              PT = sum(pt, na.rm = TRUE), 
              MORENA = sum(morena, na.rm = TRUE), 
              HUMANISTA = sum(humanista, na.rm = TRUE), 
              PES = sum(encuentro_social, na.rm = TRUE), 
              noREG = sum(candidato_no_registrado, na.rm = TRUE), 
              NULOS = sum(votos_nulos, na.rm = TRUE), 
              Total = sum(votacion_total_emitida, na.rm = TRUE))
  df.temporal <- left_join(df.temporal, df.estratos) 
  df.temporal <- df.temporal %>%
    mutate(Wh = vecNh / vecnh)
  
  estTotal = df.temporal$Wh %*% df.temporal$Total %>% c()
  estPAN = df.temporal$Wh %*% df.temporal$PAN / estTotal %>% c()
  estPRI = df.temporal$Wh %*% df.temporal$PRI / estTotal %>% c()
  estPRD = df.temporal$Wh %*% df.temporal$PRD / estTotal %>% c()
  estPVEM = df.temporal$Wh %*% df.temporal$PVEM / estTotal %>% c()
  estPT = df.temporal$Wh %*% df.temporal$PT / estTotal %>% c()
  estMORENA = df.temporal$Wh %*% df.temporal$MORENA / estTotal %>% c()
  estHUMANISTA = df.temporal$Wh %*% df.temporal$HUMANISTA / estTotal %>% c()
  estPES = df.temporal$Wh %*% df.temporal$PES / estTotal %>% c()
  estnoREG = df.temporal$Wh %*% df.temporal$noREG / estTotal %>% c()
  estNULOS = df.temporal$Wh %*% df.temporal$NULOS / estTotal %>% c()
  
  resultado <- cbind(estPRI, estPAN, estPRD, estPVEM,
                     estPT, estMORENA, estHUMANISTA, estPES,
                     estnoREG, estNULOS)
  colnames(resultado) <- c("estPRI", "estPAN", "estPRD", 
                           "estPVEM", "estPT", "estMORENA", 
                           "estHUMANISTA", "estPES", "estnoREG", 
                           "estNULOS")
  
  resultado <- resultado %>%
    as.data.frame()
  rm()
  return(resultado)
}

#resultadoOtrosPartidos(df0, df.distritosTipo, muestra)




