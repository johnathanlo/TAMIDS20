require(forecast)
required(dplyr)
require(xts)
require(zoo)
require(quantmod)
require(tidyquant)
require(tseries)
require(ggplot)
require(foreign)
require(plm)

## Multi-Seasonal Time Series 
#dshw()
multi_season = msts(hourly_delay_ts, seasonal.periods = c(19,19*7)) #hourly AND weekly
summary(multi_season)
#autoplot(multi_season, main ="Multi-Seasonal Time-Series (Hourly and Weekly Seasonal Periods)", ylab="Arrival Delay (minutes)", xlab = "Time (~Weeks Since Jan 1, 2018)" )
forecast_multi = forecast(multi_season, h=1596)
#plot(forecast_multi, xlim=c(0,80))


g1 <- autoplot(forecast_multi) + 
  ggtitle("Multi-Seasonal Time-Series (Hourly and Weekly Seasonal Periods): Q3 Forecast") + 
  ylab("Median Arrival Delay (minutes)") +
  xlab("Time (Weeks Since Jan 1, 2018)")+
  coord_cartesian(xlim = c(75, 82), ylim=c(-20,25))
plot(g1)

#####################3
FlightDelaysBootstrap = sample_n(FlightDelaysBootstrap, 10)
multi_season = msts(FlightDelaysBootstrap$ARR_DELAY, seasonal.periods = c(19,19*7,19*91.3125, 19*365.25)) #hourly AND weekly
summary(multi_season)

g1<- autoplot(multi_season_decomp) + 
  ggtitle("Multi-season forecast") + 
  xlab("Time in years") +
  ylab("Arrival time")+
  coord_cartesian(xlim = c(5,6))

plot(g1)
plot(forecast_multi)
multi_season_decomp = decompose(multi_season)
multi_season_model = auto.arima(multi_season, D = 1)
plot(decompose(multi_season))
forecast_multi = forecast(multi_season, h=1596)
g1 <- autoplot(forecast_multi) + 
  ggtitle("Hourly and Weekly Seasons: Forecast") + 
  ylab("y") +
  coord_cartesian(xlim = c(75, 82))
plot(g1)

######################################## HOURLY DELAY

time_series_hourly = group_by(FlightDelays, FL_DATE, DEP_TIME_BLK) %>% summarise(median_hourly_delay = median(ARR_DELAY, na.rm=TRUE))

hourly_delay_ts = ts(time_series_hourly$median_hourly_delay, deltat = 1/19)
autoplot(hourly_delay_ts, main ="Time Series of Median Arrival Delay", xlab = "Time (Day since Jan 1, 2018)", ylab = "Arrival Delay (minutes)")
decompose(hourly_delay_ts)
plot(decompose(hourly_delay_ts))

arima_fit_hourly = auto.arima(hourly_delay_ts, D=1)
summary(arima_fit_hourly)

acf(hourly_delay_ts, main = "ACF Plot: Daily Time Series")
pacf(hourly_delay_ts, main = "PACF Plot: Daily Time Series")


#findfrequency(daily_delay_ts)
forecast_hourly = forecast(arima_fit_hourly, h=19*7*12)
plot(forecast_hourly)

# Check Residuals
plot(forecast$residuals, main = "Residuals", xlab = "Week Since Jan 1, 2018")
qqnorm(forecast$residuals) #We assume the errors to be independently distributed with the normal distribution.
plot(forecast$fitted , forecast$residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")

g2 <- autoplot(forecast_hourly) + 
  ggtitle("Q3 Forecast (Daily)") + 
  ylab("Median Arrival Delay (minutes)") + xlab("Time (Days Since Jan 1, 2018)")+
  coord_cartesian(xlim = c(540, 560), ylim = c(-15, 20))
plot(g2)

########################FlightDelaysBootstrap


hourly_delay_bootstrap = ts(FlightDelaysBootstrap$ARR_DELAY, deltat = 1/19)
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



## TIME SERIES FOR MEDIAN WEEKLY DELAY
median_daily_delay = group_by(FlightDelays, FL_DATE) %>% summarise(median_daily_delay = median(ARR_DELAY, na.rm=TRUE))


daily_delay_ts = ts(median_daily_delay$median_daily_delay, delta=1/7)
autoplot(daily_delay_ts)
decompose(daily_delay_ts)
plot(decompose(daily_delay_ts))


arima_fit_daily = auto.arima(daily_delay_ts, D=2)
summary(arima_fit_daily)

acf(daily_delay_ts, main = "ACF Plot: Weekly Time Series")
pacf(daily_delay_ts, main = "PACF Plot: Weekly Time Series")

#findfrequency(daily_delay_ts)
forecast = forecast(arima_fit_daily, h=63)
plot(forecast)



g3 <- autoplot(forecast) + 
  ggtitle("Q3 Forecast (Weekly)") + 
  ylab("Median Arrival Delay (minutes)") + 
  xlab("Time (Weeks Since Jan 1, 2018)")+
  coord_cartesian(xlim = c(70, 83), ylim=c(-30,10))
plot(g3)

#Detect lag (?), serial dependence
#Autocorrelation is a type of serial dependence. Specifically, autocorrelation is when a time series is linearly related to a lagged version of itself. By contrast, correlation is simply when two independent variables are linearly related.
#. As we can infer from the graph above, the autocorrelation continues to decrease as the lag increases, confirming that there is no linear association between observations separated by larger lags.

# we need three variables: p, d, and q which are non-negative integers that refer to the order of the autoregressive, integrated, and moving average parts of the model respectively.

acf(hourly_delay_ts)
pacf(hourly_delay_ts)

# Check Residuals
plot(forecast$residuals)
qqnorm(forecast$residuals) #We assume the errors to be independently distributed with the normal distribution.
acf(forecast$residuals) #autocorrelation function
pacf(forecast$residuals) #partial autocoreelation

plot(forecast$fitted , forecast$residuals)

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








##################################################     LONGITUDINAL DATA/PANEL DATA/ CROSS-sectional time series data 
coplot(ARR_DELAY ~ FL_DATE|DEST, type='l', data = FlightDelays_Full)


