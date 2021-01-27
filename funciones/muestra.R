muestra <- function(t_poblacion){
 
   e <- 0.05
   Z <- 1.96 # Valor en tabla de distribucion normal para el 95% de confianza y 5% de error
   p <- 0.50
   q <- 0.50 # Igual que (1-p)
   
   t_muestra <- (Z^2 * p * q * t_poblacion) / (((t_poblacion-1) * e^2) + (Z^2 * p * q))
   
   t_muestra <- round(t_muestra)
  
   return(t_muestra)
   
}
