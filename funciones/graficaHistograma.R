graficaHistograma <-function(df.simulacion, vtitulo){
  M <- length(df.simulacion[,1])
  
  comp <- ggplot(data=df.simulacion) +
    geom_histogram(aes(x=PRI, fill="PRI"),color="red",  alpha=0.5)+
    geom_histogram(aes(x=PAN, fill="PAN"), color="blue",  alpha=0.5)+
    scale_fill_manual(breaks = c("PRI", "PAN"),
                      values=c("red", "blue")) +
    theme_bw() + Tema +
    labs(fill = "Partido", color= NULL, y = "Densidad", x = "% votación", title = vtitulo,
         caption = NULL, subtitle = paste("Resultados de",M, "simulaciones"))
  show(comp)
}