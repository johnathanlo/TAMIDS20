PredictDelays <- function(obs){
  #########Function needs the following data loaded to work
  ##load("data/arima_fit_hourly.RData")
  ##load("data/arima_fit.RData")
  ##load("data/Rdata/residual_linear_model.RData")
  days = 0
  if(obs$MONTH == 8){
    days = 31
  }else if(obs$MONTH == 9){
    days = 62
  }else if(obs$MONTH==10){
    days = 92
  }
  days = days + as.numeric(obs$DAY_OF_MONTH)
  steps = days*19+as.numeric(obs$DEP_TIME_BLK)
  
  obs$MONTH <- as.factor(obs$MONTH)
  obs$QUARTER<- as.factor(obs$QUARTER)
  hresid<-predict(arima_fit_hourly, n.ahead = steps)
  obs$arima_hourly_res = hresid$pred[steps]
  
  eta <- predict(arima_fit, n.ahead = steps)
  obs$arima_weekly_res = eta$pred[steps]
  lin <- predict(lm_res_weekly,newdata = obs )
  
  return (eta+lin)
}


