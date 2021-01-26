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

sistcomp <- ggplot(data=simulacionSIST) +
  geom_density(aes(x=PRI, fill="PRI"),color="red",  alpha=0.5)+
  geom_density(aes(x=PAN, fill="PAN"), color="blue", alpha=0.5)+
  scale_fill_manual(breaks = c("PRI", "PAN"),
                    values=c("red", "blue")) +
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Densidad", x = "% votación", title = "Simulación: Muestreo sistemático",
       caption = NULL, subtitle = "Resultados de mil simulaciones")
ggsave("Gráficos/sistcomp.png",sistcomp, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

maserror <- ggplot(data=simulacionMAS, aes(x=dif.PRI^2)) +
  geom_histogram(bins=50, color="red", fill = "red", alpha=0.5)+
  scale_y_continuous(labels = comma) + 
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Observaciones", x = "Diferencia al cuadrado", title = "Simulación: Muestreo aleatorio simple",
       caption = NULL, subtitle = "Errores de mil simulaciones respecto al resultado observado")
ggsave("Gráficos/maserror.png",maserror, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

sisterror <- ggplot(data=simulacionSIST, aes(x=dif.PRI^2)) +
  geom_histogram(bins=50, color="red", fill = "red", alpha=0.5)+
  scale_y_continuous(labels = comma) + 
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Observaciones", x = "Diferencia al cuadrado", title = "Simulación: Muestreo sistemático",
       caption = NULL, subtitle = "Errores de mil simulaciones respecto al resultado observado")
ggsave("Gráficos/sisterror.png",sisterror, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')


estrerror <- ggplot(data=simulacionESTR.simple, aes(x=dif.PRI^2)) +
  geom_histogram(bins=50, color="red", fill = "red", alpha=0.5)+
  scale_y_continuous(labels = comma) + 
  theme_bw() + Tema +
  labs(fill = "Partido", color= NULL, y = "Observaciones", x = "Diferencia al cuadrado", title = "Simulación: Muestreo estratificado",
       caption = NULL, subtitle = "Errores de mil simulaciones respecto al resultado observado")
ggsave("Gráficos/estrerror.png",estrerror, bg = "transparent", height = 12, width = 18, units = "cm", dpi = 800, type = 'cairo')

