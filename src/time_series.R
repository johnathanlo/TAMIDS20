require(forecast)
required(dplyr)

## TIME SERIES FOR MEDIAN DAILY DELAY
median_daily_delay = group_by(FlightDelays, FL_DATE) %>% summarise(avg_daily_delay = median(ARR_DELAY, na.rm=TRUE))

daily_delay_ts = ts(median_daily_delay$avg_daily_delay)
str(daily_delay_ts)
decompose(daily_delay_ts, c("additive", "multiplicative"))

par(mfrow=c(1,1))
plot(daily_delay_ts)

arima_fit_daily = auto.arima(daily_delay_ts)
summary(arima_fit_daily)

forecast = forecast(arima_fit_daily, h =100)
plot(forecast)

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

