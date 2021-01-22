calculaECM <- function(x){
  x <- x %>%
    mutate(dif.PRI = PRI - Global$PRI,
           dif.PAN = PAN - Global$PAN,
           dif.PRD = PRD - Global$PRD) %>%
    summarise(ECM.PRI = sum(dif.PRI^2)/M,
              ECM.PAN = sum(dif.PAN^2)/M,
              ECM.PRD = sum(dif.PRD^2)/M)
  return(x)
}