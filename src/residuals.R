require(dplyr)
require(ggpairs)
require(ggally)
require(psych)
require(leaps)
require(olsrr)
require(broom)

load("data/Rdata/arima_fit_hourly.RData")
load("data/Rdata/arima_fit_weekly.RData")
load("data/Rdata/FlightDelaysBootstrap.RData")

FlightDelaysBootstrap = ungroup(FlightDelaysBootstrap)
FlightDelaysBootstrap$arima_weekly_res = as.vector(arima_fit_weekly$residuals)
FlightDelaysBootstrap$arima_hourly_res = as.vector(arima_fit_hourly$residuals)

## Generate OLS model 
# data = data frame with covariates
# n = number of models to be fit
# sample.size = size of each subset
# returns list of 2: coef_samples (all estimates over n subset), coef_avg (avg of coefficients)
generate_OLS_coef <- function(data, n, sample.size) {
  for (i in 1:n) {
    set.seed(2020+i)
    subset_residuals = sample_n(data, sample.size)
    wts = c(sqrt((subset_residuals$prcp)+1))
    lm_res_weekly = lm(arima_weekly_res ~ . -ARR_DELAY -Route -YEAR -QUARTER -MONTH -DAY_OF_MONTH -DAY_OF_WEEK -DEP_TIME_BLK -ARR_TIME_BLK, weights = wts, data = subset_residuals)
    #create data frame
    if (i==1) {
      coefficients = as.data.frame(tidy(lm_res_weekly)[,1:2])
      names(coefficients)[2] = "estimate_1"
    }
    #update data frame
    else {
      col_name <- paste("estimate_", i, sep = "")
      coefficients = merge(coefficients, as.data.frame(tidy(lm_res_weekly)[,1:2]), by = "term", all.x=TRUE)
      names(coefficients)[i+1]=col_name
    }
  }
  coefficient_avg =  rowwise(coefficients) %>% 
    mutate(coef_avg=mean(c(estimate_1,estimate_2, estimate_3, estimate_4, estimate_5))) %>%
      select(term, coef_avg) %>%
        rename(predictor = term)
  return(list(coef_samples = as.data.frame(coefficients), coef_avg = as.data.frame(coefficient_avg)))
}

# For our additive model 
OLS_estimates = generate_OLS_coef(FlightDelaysBootstrap,5, 50000)

linear_final = OLS_estimates$coef_avg
save(list = c("linear_final"), file = "data/Rdata/linear_final.RData")

##############################################################################################
## see if we should fit another model on the residuals of the residuals
subset_residuals$res_of_res = as.vector(lm_res_weekly$residuals)
lm_res_of_res = lm(res_of_res ~ .  -ARR_DELAY -Route -YEAR -QUARTER -MONTH -DAY_OF_MONTH -DAY_OF_WEEK -DEP_TIME_BLK -ARR_TIME_BLK -arima_weekly_res -arima_hourly_res, data = subset_residuals)
summary(lm_res_of_res) ## we see doing this doesn't explain any more variation in the data, so we stop 
plot(lm_res_of_res)

##############################################################################################
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
