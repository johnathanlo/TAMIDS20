require(forecast)
required(dplyr)
require(xts)
require(zoo)
require(quantmod)
require(tidyquant)
require(tseries)

##try to get residuals

timeseries_lm = lm(ARR_DELAY ~. -Route, data = sample_frac(FlightDelays_RF,0.05))





## TIME SERIES FOR MEDIAN DAILY DELAY
median_daily_delay = group_by(FlightDelays, FL_DATE) %>% summarise(median_daily_delay = median(ARR_DELAY, na.rm=TRUE))

median_daily_delay = rbind(median_daily_delay, median_daily_delay)
daily_delay_ts = ts(median_daily_delay$median_daily_delay, deltat=1/365)
autoplot(daily_delay_ts)
str(daily_delay_ts)
decompose(daily_delay_ts)
plot(decompose(daily_delay_ts))

par(mfrow=c(1,1))
plot(daily_delay_ts)

arima_fit_daily = auto.arima(daily_delay_ts, stepwise=TRUE)
summary(arima_fit_daily)

forecast = forecast(arima_fit_daily, h =200)
plot(forecast)

###### backup: exponential smoothing (assuming no seasonality)
Delay.Forecasts = HoltWinters(median_daily_delay, beta=FALSE, gamma = FALSE)
Delay.Forecasts
plot(Delay.Forecasts)
summary(Delay.Forecasts)
Delay.Forecasts$SSE


## TIME SERIES FOR AVG DAILY DELAY
avg_monthly_delay = group_by(FlightDelays, MONTH) %>% summarise(avg_daily_delay = mean(ARR_DELAY, na.rm=TRUE))

monthly_delay_ts = ts(avg_monthly_delay$avg_daily_delay)
str(monthly_delay_ts)
decompose(monthly_delay_ts, c("additive", "multiplicative"))

par(mfrow=c(1,1))
plot(monthly_delay_ts)

arima_fit_monthly = auto.arima(monthly_delay_ts)
summary(arima_fit_monthly)

forecast = forecast(arima_fit_monthly, h =6)
plot(forecast)

###############
ggAcf(daily_delay_ts)
adf.test(daily_delay_ts) ##Augmented Dickey-Fuller Test


########### Fit time series to residuals
summary(numflights_fit)
names(numflights_fit)
numflights_fit$residuals
residuals_fit = numflights_fit$residuals
