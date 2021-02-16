lecturaDatos <- function(){
  x<- read_excel("data/ComputoGobernador2015_Casilla.xlsx", 
                 sheet = "POR CASILLA", skip = 1) %>%
    clean_names()
  UbicacionCasillas2015 <- read_excel("data/UbicacionCasillas2015.xlsx", skip = 4) %>%
    clean_names()
  casillas_sonora <- read.csv("data/casillas_sonora.csv")  %>%
    select(seccion, casilla_tipo) %>%
    distinct(seccion, casilla_tipo) %>%
    rbind(c(608, "RURAL"))
  casillas_sonora$seccion %<>% as.numeric()
  
  x <- UbicacionCasillas2015 %>%
    left_join(x, ., by = c("seccion", "casilla")) %>%
    select(-c(distrito_local.y, municipio.y, domicilio)) %>%
    mutate(seccion_casilla = paste(seccion, casilla)) %>%
    rename(municipio = municipio.x,
           distrito_local = distrito_local.x)
  
  x <- casillas_sonora %>%
    left_join(x, ., by = "seccion") %>%
    mutate(distrito_tipo = paste(distrito_local,casilla_tipo))
  rm(casillas_sonora)
  
  return (x)
}