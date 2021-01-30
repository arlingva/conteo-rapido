graficaError <-function(df.simulacion, vtitulo){
  M <- length(df.simulacion[,1])
  error <- ggplot(data=df.simulacion, aes(x=dif.PRI^2)) +
    geom_histogram(bins=50, color="red", fill = "red", alpha=0.5)+
    scale_y_continuous(labels = comma) +
    theme_bw() + Tema +
    labs(fill = "Partido", color= NULL, 
         x = "Diferencia al cuadrado", 
         y = "Observaciones", 
         title = vtitulo,
         caption = NULL, 
         subtitle = paste("Errores de", M, "simulaciones respecto al resultado observado"))
  show(error)
}