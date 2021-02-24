funcionSimulaEstrVar <- function(df, n, M, df.distritos){
  distritos <- df.distritos$distrito_local
  Nh <- df.distritos$Nh
  Nh <- df.distritos$Nh
  L <- length(distritos)
  N <-sum(Nh)
  
  # Crea una matriz vacía para el resultado
  resultado <- data.frame()
  
  # aquí se guardarán el estimador y varianza de cada partido, de cada muestra
  estPRI <-rep(0, M)
  estPAN <-rep(0, M)
  varPRI <-rep(0, M)
  varPAN <-rep(0, M)
  i<-4
  for(i in 1:M){
    muestra <- c() 
    # en este ciclo se selecciona la muestra dentro de cada estrato
    for(j in 1: L){
      df %>%
        filter(distrito_local == distritos[j]) %>%
        select(seccion_casilla) -> df.muestra
      muestra <- c(muestra, sample(df.muestra$seccion_casilla, df.distritos$Nh[j]))
    } 
    
    # Se calcula el porcentaje de votos de cada estrato para cada muestra
    
    df.temporal <- df %>%
      filter(seccion_casilla %in% muestra) %>%
      group_by(distrito_local) %>% 
      summarise(PRI = sum(total_coalicion)/sum(votacion_total_emitida),
                PAN = sum(pan) / sum(votacion_total_emitida)) 

    estPRI[i] = Nh %*% df.temporal$PRI / N
    estPAN[i] = Nh %*% df.temporal$PAN / N
    varPRI[i] = sum((1-Nh/Nh) * 
                      (Nh/N)^2 *
                      (df.temporal$PRI*(1-df.temporal$PRI)) / (Nh-1))
    varPAN[i] = sum((1-Nh/Nh) * 
                      (Nh/sum(Nh))^2 *
                      (df.temporal$PAN*(1-df.temporal$PAN)) / (Nh-1))
  }
  
  resultado <- cbind(estPRI, estPAN, varPRI, varPAN)
  colnames(resultado) <- c("estPRI", "estPAN", "varPRI", "varPAN")
  resultado <- resultado %>%
    as.data.frame() %>%
    mutate(dif.PRI = estPRI - resultadoReal$PRI,
           dif.PAN = estPAN - resultadoReal$PAN)
  return(resultado)  
}