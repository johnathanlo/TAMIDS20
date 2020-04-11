PredictDelays <- function(obs){
  load("data/arima_fit_hourly.RData")
  load("data/arima_fit.RData")
  load("data/linear_final.RData")
  
  if(obs$MONTH == 8){
    days = 31
  }else if(obs$MONTH == 9){
    days = 62
  }else if(obs$MONTH==10){
    days = 92
  }
  days = days + obs$DAY_OF_MONTH
  steps = days*19+as.numeric(obs$DEP_TIME_BLK)
  
  hresid<-predict(arima_fit_hourly, n.ahead = steps)
  obs$arima_hourly_res = resid
  
  eta <- predict(arima_fit, n.ahead = steps)
  
  lin <- predict(linear_final,newdata = obs )
  
  return (eta+lin)
}


