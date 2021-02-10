calculaECM <- function(x){
  x <- x %>%
    summarise(ECM.PRI = sum(dif.PRI^2)/M,
              ECM.PAN = sum(dif.PAN^2)/M)
  return(x)
}