library(tidyverse)
library(extrafont)
library(scales)
library(showtext)
library(tint)
library(miniUI)
library(units)
library(ggfortify)
library(cowplot)
library("Cairo")

# Carga de fuentes por medio del paquete showtext para Google Fonts
font_add_google("Lato", "Lato")
font_add_google("Lato", "Lato Black")
font_add_google("Lato", "Lato Light")


Tema <- theme(plot.title = (element_text(family = "Lato Black", size = 20, color = "black")),
              plot.subtitle = (element_text(family = "Lato", size = 10, color = "black")),
              legend.position = "top",  plot.margin = margin(0.5, 1, 0.5, 1, "cm"), 
              legend.key.height = unit (0.2, "cm"), legend.key.width = unit (0.2, "cm"),  
              axis.text = element_text(family = "Lato Light", size = 10, color = "black"),
              legend.text = element_text(family = "Lato Light", size = 8, color = "black"),
              legend.title = element_text(family = "Lato Black", size = 10, color = "black"),
              plot.caption = element_text(family = "Lato Light", size = 10, color = "black", face="italic"),
              axis.title = element_text(family = "Lato", size = 12, color = "black"))

mascomp <- ggplot(data=simulacionMAS) +
  geom_density(aes(x=PRI, fill="PRI"),color="red",  alpha=0.5)+
  geom_density(aes(x=PAN, fill="PAN"), color="blue",  alpha=0.5)+
    scale_fill_manual(breaks = c("PRI", "PAN"),
                       values=c("red", "blue")) +
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Densidad", x = "% votación", title = "Simulación: Muestreo aleatorio simple",
       caption = NULL, subtitle = "Resultados de mil simulaciones")
ggsave("Gráficos/mascomp.png",mascomp, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

estcomp <- ggplot(data=simulacionESTR.simple) +
  geom_density(aes(x=PRI, fill="PRI"),color="red", alpha=0.5)+
  geom_density(aes(x=PAN, fill="PAN"), color="blue", alpha=0.5)+
  scale_fill_manual(breaks = c("PRI", "PAN"),
                    values=c("red", "blue")) +
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Densidad", x = "% votación", title = "Simulación: Muestreo estratificado",
       caption = NULL, subtitle = "Resultados de mil simulaciones")
ggsave("Gráficos/estcomp.png",estcomp, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

maserror <- ggplot(data=simulacionMAS, aes(x=dif.PRI^2)) +
  geom_histogram(bins=50, color="red", fill = "red", alpha=0.5)+
  scale_y_continuous(labels = comma) + 
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Observaciones", x = "Diferencia al cuadrado", title = "Simulación: Muestreo aleatorio simple",
       caption = NULL, subtitle = "Errores de mil simulaciones respecto al resultado observado")
ggsave("Gráficos/maserror.png",maserror, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

estrerror <- ggplot(data=simulacionESTR.simple, aes(x=dif.PRI^2)) +
  geom_histogram(bins=50, color="red", fill = "red", alpha=0.5)+
  scale_y_continuous(labels = comma) + 
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Observaciones", x = "Diferencia al cuadrado", title = "Simulación: Muestreo estratificado",
       caption = NULL, subtitle = "Errores de mil simulaciones respecto al resultado observado")
ggsave("Gráficos/estrerror.png",estrerror, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')


estrerrorprop <- ggplot(data=simulacionESTR.prop, aes(x=dif.PRI^2)) +
  geom_histogram(bins=50, color="red", fill = "red", alpha=0.5)+
  scale_y_continuous(labels = comma) +
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Observaciones", x = "Diferencia al cuadrado", title = "Simulación: Muestreo estratificado Rural/Urbano",
       caption = NULL, subtitle = "Errores de mil simulaciones respecto al resultado observado")
ggsave("Gráficos/estrerrorprop.png",estrerror, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

resumen <- resumen %>% tibble::rownames_to_column()  %>% rename(muestreo=rowname)
resumen2 <- data.frame(t(resumen[-1]))
colnames(resumen2) <- resumen[, 1]

intervaloMAS <- simulacionMAS %>% summarise(PRI= mean(PRI), PAN=mean(PAN), PRD=mean(PRD)) %>% 
  gather("Partido", "Promedio") %>% mutate(Máximo=Promedio+(3*(sqrt(as.double(resumen2$MAS)))), Mínimo=Promedio-(3*(sqrt(as.double(resumen2$MAS))))) %>% 
  filter(Partido!="PRD")
intervaloestr <- simulacionESTR.simple %>% summarise(PRI= mean(PRI), PAN=mean(PAN), PRD=mean(PRD))%>% 
  gather("Partido", "Promedio") %>%  mutate(Máximo=Promedio+(3*sqrt(as.double(resumen2$Sistemático))), Mínimo=Promedio-(3*sqrt(as.double(resumen2$Sistemático)))) %>% 
  filter(Partido!="PRD")
intervaloestrprop <- simulacionESTR.prop %>% summarise(PRI= mean(PRI), PAN=mean(PAN), PRD=mean(PRD))%>%
  gather("Partido", "Promedio") %>%  mutate(Máximo=Promedio+(3*sqrt(as.double(resumen2$Sistemático))), Mínimo=Promedio-(3*sqrt(as.double(resumen2$Sistemático)))) %>%
  filter(Partido!="PRD")




intervalMAS <- ggplot(data=intervaloMAS, aes(y=Partido)) +
  geom_point(aes(x=Promedio),color="red",fill="red",  alpha=0.5)+
  geom_point(aes(x=Mínimo),color="blue", fill="blue", alpha=0.5)+
  geom_point(aes(x=Máximo),color="blue", fill="blue",  alpha=0.5)+
  theme_bw() + Tema +
  labs(fill = NULL, color= NULL, y = "% votación", x = "Partido", title = "Simulación: Muestreo aleatorio simple",
       caption = NULL, subtitle = "Intervalo de confianza")
ggsave("Gráficos/masinter.png",intervalMAS , bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

intervalestr <- ggplot(data=intervaloestr, aes(y=Partido)) +
  geom_point(aes(x=Promedio),color="red",fill="red",  alpha=0.5)+
  geom_point(aes(x=Mínimo),color="blue", fill="blue", alpha=0.5)+
  geom_point(aes(x=Máximo),color="blue", fill="blue",  alpha=0.5)+
  theme_bw() + Tema +
  labs(fill = NULL, color= NULL, y = "% votación", x = "Partido", title = "Simulación: Muestreo estratificado",
       caption = NULL, subtitle = "Intervalo de confianza")
ggsave("Gráficos/estrinter.png",intervalestr, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

intervalestrprop <- ggplot(data=intervaloestrprop, aes(y=Partido)) +
  geom_point(aes(x=Promedio),color="red",fill="red",  alpha=0.5)+
  geom_point(aes(x=Mínimo),color="blue", fill="blue", alpha=0.5)+
  geom_point(aes(x=Máximo),color="blue", fill="blue",  alpha=0.5)+
  theme_bw() + Tema +
  labs(fill = NULL, color= NULL, y = "% votación", x = "Partido", title = "Simulación: Muestreo estratificado R/U",
       caption = NULL, subtitle = "Intervalo de confianza")
ggsave("Gráficos/estrinterprop.png",intervalestr, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')


