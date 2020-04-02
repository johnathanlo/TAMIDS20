require(forecast)
required(dplyr)
require(xts)
require(zoo)
require(quantmod)
require(tidyquant)
require(tseries)
require(ggplot)
## Multi-Seasonal Time Series 
dshw()
multi_season = msts(hourly_delay_ts, seasonal.periods = c(19,19*7)) #hourly AND weekly
summary(multi_season)
forecast_multi = forecast(multi_season, h=1596)
plot(forecast_multi)


g1 <- autoplot(forecast_multi) + 
  ggtitle("Hourly and Weekly Seasons: Forecast") + 
  ylab("y") +
  coord_cartesian(xlim = c(75, 82))
plot(g1)



########################################33

time_series_weekly = group_by(FlightDelays, FL_DATE, DEP_TIME_BLK) %>% summarise(median_hourly_delay = median(ARR_DELAY, na.rm=TRUE))

hourly_delay_ts = ts(time_series_weekly$median_hourly_delay, deltat = 1/19)
autoplot(hourly_delay_ts)
decompose(hourly_delay_ts)
plot(decompose(hourly_delay_ts))

arima_fit_hourly = auto.arima(hourly_delay_ts, D=1)
summary(arima_fit_hourly)

#findfrequency(daily_delay_ts)
forecast_hourly = forecast(arima_fit_hourly, h=1596)
plot(forecast_hourly[])

g2 <- autoplot(forecast_hourly) + 
  ggtitle("Hourly forecast") + 
  ylab("y") +
  coord_cartesian(xlim = c(545, 640))
plot(g2)



## TIME SERIES FOR MEDIAN DAILY DELAY
median_daily_delay = group_by(FlightDelays, FL_DATE) %>% summarise(median_daily_delay = median(ARR_DELAY, na.rm=TRUE))

median_daily_delay = rbind(median_daily_delay, median_daily_delay)
daily_delay_ts = ts(median_daily_delay$median_daily_delay, deltat=1/7)
autoplot(daily_delay_ts)
str(daily_delay_ts)
decompose(daily_delay_ts)
plot(decompose(daily_delay_ts))

par(mfrow=c(1,1))
plot(daily_delay_ts)

arima_fit_daily = auto.arima(daily_delay_ts, D=2)
summary(arima_fit_daily)
accuracy(arima_fit_daily)

findfrequency(daily_delay_ts)
forecast = forecast(arima_fit_daily, h=12)
plot(forecast)



g3 <- autoplot(forecast) + 
  ggtitle("Weekly forecast") + 
  ylab("y") +
  coord_cartesian(xlim = c(75, 82))
plot(g3)

#Detect lag (?)
acf(daily_delay_ts)
pacf(daily_delay_ts)

# Check Residuals
plot(forecast$residuals)
qqnorm(forecast$residuals) #We assume the errors to be independently distributed with the normal distribution.
acf(forecast$residuals) #autocorrelation function
pacf(forecast$residuals) #partial autocoreelation



###### backup: exponential smoothing (assuming no seasonality)
Delay.Forecasts = HoltWinters(median_daily_delay, beta=FALSE, gamma = FALSE)
Delay.Forecasts
plot(Delay.Forecasts)
summary(Delay.Forecasts)
Delay.Forecasts$SSE


## TIME SERIES FOR AVG DAILY DELAY
avg_monthly_delay = group_by(FlightDelays, MONTH) %>% summarise(avg_monthly_delay = mean(ARR_DELAY, na.rm=TRUE))

avg_monthly_delay = rbind(avg_monthly_delay, avg_monthly_delay)
monthly_delay_ts = ts(avg_monthly_delay$avg_monthly_delay)
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


