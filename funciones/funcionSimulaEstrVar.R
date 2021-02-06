funcionSimulaEstrVar <- function(df, n, M, df.distritos){
  distritos <- df.distritos$distrito_local
  N.dist <- df.distritos$N.dist
  n.dist <- df.distritos$n.dist
  L <- length(distritos)
  
  # Crea una matriz vacía para el resultado
  resultado <- data.frame()
  
  # aquí se guardarán el estimador y varianza de cada partido, de cada muestra
  estPRI<-rep(0, M)
  estPAN<-rep(0, M)
  varPRI<-rep(0, M)
  varPAN<-rep(0, M)
  
  for(i in 1:M){
    muestra <- c() 
    # en este ciclo se selecciona la muestra dentro de cada estrato
    for(j in 1: L){
      df %>%
        filter(distrito_local == distritos[j]) %>%
        select(seccion_casilla) -> df.muestra
      muestra <- c(muestra, sample(df.muestra$seccion_casilla, df.distritos$n.dist[j]))
    } 
    
    # Se calcula el porcentaje de votos de cada estrato para cada muestra
    
    df.temporal <- df %>%
      filter(seccion_casilla %in% muestra) %>%
      group_by(distrito_local) %>% 
      summarise(PRI = sum(total_coalicion)/sum(votacion_total_emitida),
                PAN = sum(pan) / sum(votacion_total_emitida)) 
    
    # para cada muestra se calcula el estimador y la varianza
    
    estPRI[i] = N.dist %*% df.temporal$PRI / N
    estPAN[i] = N.dist %*% df.temporal$PAN / N
    varPRI[i] = sum((1-n.dist/N.dist) * 
                      (N.dist/sum(N.dist))^2 *
                      (estPRI[i]*(1-estPRI[i])) / (n.dist-1))
    varPAN[i] = sum((1-n.dist/N.dist) * 
                      (N.dist/sum(N.dist))^2 *
                      (estPAN[i]*(1-estPAN[i])) / (n.dist-1))
  }
  
  resultado <- cbind(estPRI, estPAN, varPRI, varPAN)
  colnames(resultado) <- c("estPRI", "estPAN", "varPRI", "varPAN")
  resultado <- resultado %>%
    as.data.frame() %>%
    mutate(dif.PRI = estPRI - resultadoReal$PRI,
           dif.PAN = estPAN - resultadoReal$PAN)
  return(resultado)  
}