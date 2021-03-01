calculaDelta <- function(df, df.estratos, muestra, partido, est){
  #df <- df0  # para pruebas
  #df.estratos <- df.distritosTipo # para pruebas
  #muestra<-muestra0 # para pruebas
  #est <- estPAN
  #partido <- df2$pan
  vecnh <- df.estratos$nh
  vecNh <- df.estratos$Nh
  
  df2 <- df %>%
    filter(seccion_casilla %in% muestra)

  df2 <- df2 %>%
    mutate(Ghi = (partido - est*votacion_total_emitida) / estTotal)
  df2 %>%
    group_by(estrato) %>%
    summarise(Gh.barra = mean(Ghi, na.rm = TRUE)) -> Gh.barra
  
  df2 <- df2 %>%
    left_join(Gh.barra) %>%
    mutate(Ghi_Gh = (Ghi - Gh.barra)^2)
  
  df2 %>%
    group_by(estrato) %>%
    summarise(numerador = sum(Ghi_Gh)) %>%
    left_join(df.estratos) %>%
    mutate(denominador = vecnh - 1,
           VarGhi = numerador / denominador) %>%
    select(VarGhi) -> VarGhi
  
  varianza <- sum(vecNh^2 * (1/vecnh - 1/vecNh) * VarGhi, na.rm = TRUE)
  
  delta <- sqrt(varianza) * z
  return(delta)
}