require(dplyr)
require(ggpairs)
require(ggally)
require(psych)
require(leaps)
require(olsrr)

FlightDelaysBootstrap = ungroup(FlightDelaysBootstrap)
FlightDelaysBootstrap$arima_weekly_res = as.vector(arima_fit_weekly$residuals)
FlightDelaysBootstrap$arima_hourly_res = as.vector(arima_fit_hourly$residuals)


str(FlightDelaysBootstrap)
subset_residuals = sample_n(FlightDelaysBootstrap, 50000)
wts = c(sqrt((subset_residuals$prcp)+1))
lm_res_weekly = lm(arima_weekly_res ~ . -ARR_DELAY -Route -YEAR -QUARTER -MONTH -DAY_OF_MONTH -DAY_OF_WEEK -DEP_TIME_BLK -ARR_TIME_BLK , weights = wts, data = subset_residuals)
summary(lm_res_weekly)
plot(lm_res_weekly)

subset_residuals$res_of_res = as.vector(lm_res_weekly$residuals)
lm_res_of_res = lm(res_of_res ~ .  -ARR_DELAY -Route -YEAR -QUARTER -MONTH -DAY_OF_MONTH -DAY_OF_WEEK -DEP_TIME_BLK -ARR_TIME_BLK -arima_weekly_res -arima_hourly_res, data = subset_residuals)
summary(lm_res_of_res)
plot(lm_res_of_res)

### Stepwise variable selection
predictors = (pairs[,-c(55)])
response = as.vector(pairs[,c(55)])
leaps = regsubsets(arima_weekly_res ~ . -ARR_DELAY, data = pairs, nbest = 10, method = "forward")
which = as.data.frame(summary(leaps)$which)


### see transformations of variables
pairs = select(subset_residuals, -c(ORIGIN, DEST, YEAR, QUARTER, MONTH, DAY_OF_MONTH, DAY_OF_WEEK, CARRIER, Route, DEP_TIME_BLK, ARR_TIME_BLK, carrier_lg, carrier_low))

pairs_subset = pairs[,c(1:10, 55)]
pairs.panels(pairs_subset)

pairs_subset = pairs[,c(11:20, 55)]
pairs.panels((pairs_subset))

pairs_subset = pairs[,c(21:30, 55)]
pairs.panels((pairs_subset))

pairs_subset = pairs[,c(31:42, 55)]
pairs.panels((pairs_subset))

pairs_subset = pairs[,c(43:54,56,57, 55)]
pairs.panels((pairs_subset))
