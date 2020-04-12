OLSpredict <- function(obs){
  ###must have this model loaded###
  load("data/Rdata/lm_flightdelays.RData")
  return(predict(lm_flightdelays, newdata = obs))
}