graficaIC <-function(vecIC, vtitulo){
  interval <- ggplot(data=vecIC, aes(y=Partido)) +
    geom_point(aes(x=Promedio),color="red",fill="red",  alpha=0.5)+
    geom_point(aes(x=Mínimo),color="blue", fill="blue", alpha=0.5)+
    geom_point(aes(x=Máximo),color="blue", fill="blue",  alpha=0.5)+
    geom_point(aes(x=c(resultadoReal$PRI, resultadoReal$PAN)),
               color="black",fill="red",  alpha=0.5)+
    theme_bw() + Tema +
    labs(fill = NULL, color= NULL, x = "% votación", y = "Partido", 
         title = vtitulo,
         caption = NULL, subtitle = "Intervalo de confianza")
  show(interval)
}

