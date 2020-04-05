require(caret)
require(dplyr)
require(aod)
require(ElemStatLearn) 


## Logistic Regression for Whether a Flight Will be Delayed
# https://stats.idre.ucla.edu/r/dae/logit-regression/

Flight_Delays = read.csv("data/FlightDelays.csv")
Flight_Delays$YEAR <- as.factor(Flight_Delays$YEAR)
Flight_Delays$QUARTER <- as.factor(Flight_Delays$QUARTER)
Flight_Delays$MONTH <- as.factor(Flight_Delays$MONTH)
Flight_Delays$DAY_OF_MONTH <- as.factor(Flight_Delays$DAY_OF_MONTH)
Flight_Delays$DAY_OF_WEEK <- as.factor(Flight_Delays$DAY_OF_WEEK)
Flight_Delays$Route <- as.factor(Flight_Delays$Route)
Flight_Delays$CANCELED <- as.factor(Flight_Delays$CANCELED)
Flight_Delays$DEP_DEL15 <- as.factor(Flight_Delays$DEP_DEL15)
Flight_Delays$ARR_DEL15 <- as.factor(Flight_Delays$ARR_DEL15)
Flight_Delays$DEP_DELAY_GROUP <- as.factor(Flight_Delays$DEP_DELAY_GROUP)
Flight_Delays$ARR_DELAY_GROUP <- as.factor(Flight_Delays$ARR_DELAY_GROUP)
Flight_Delays$DIVERTED <- as.factor(Flight_Delays$DIVERTED)

#Run only if doing modeling of if a delay will occur 
#Flight_Delays$ARR_DELAY_NEW = ifelse(Flight_Delays$ARR_DELAY_NEW > 1, 1, Flight_Delays$ARR_DELAY_NEW) 
#Flight_Delays$ARR_DELAY_NEW = as.factor(Flight_Delays$ARR_DELAY_NEW)

set.seed(Sys.time())
subset = sample_frac(Flight_Delays, size=0.05)
log_model = glm(ARR_DELAY_NEW ~ YEAR+QUARTER+MONTH+DAY_OF_WEEK+CARRIER+ORIGIN+DEST+Route+AIR_TIME+DISTANCE, data= subset, family = "binomial")

step_selection = step(log_model, scope=list(upper=~., lower=~1), k=2) 

require(ResourceSelection)
### Hosmer-Lemeshow goodness of fit test for logistic regression (large p-value means good fit)
#print p-value for different group size 
for (i in 5:15) {
  print(hoslem.test(log_model$y, fitted(log_model), g=i)$p.value)
}

#- FL_DATE - FL_NUM - ORIGIN - DEST - CRS_DEP_TIME - ARR_TIME - DEP_TIME_BLK - ARR_TIME_BLK - EMPTOTAL - EMPFTE - DIVERTED


summary(log_model) #The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable (for numerical variables)
#The coefficients for categorial variables says the change in log-odds if the observation took on that specific factor versus the factor (for that categorical variable) not shown 
confint(log_model)
wald.test(b = coef(log_model), Sigma = vcov(log_model), Terms = 2:4) #test for significance of QUARTER
## odds ratios and 95% CI
exp(cbind(OR = coef(log_model), confint(log_model))) #for a one unit increase in dependent variable, the odds of a delay ocurring (versus not occuring) increases by a factor of the coefficeint of that dependent variable (and same interpretation as above for wald.test for categorical variables)

#Test or model utiltiy:
#One measure of model fit is the significance of the overall model. This test asks whether the model with predictors fits significantly better than a model with just an intercept (i.e., a null model). The test statistic is the difference between the residual deviance for the model with predictors and the null model. The test statistic is distributed chi-squared with degrees of freedom equal to the differences in degrees of freedom between the current and the null model (i.e., the number of predictor variables in the model).

with(log_model, null.deviance - deviance) #test-statistics
with(log_model, df.null - df.residual) #df for chi-square
with(log_model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #p-value

### LOOKUP: Hosmer-Lemeshow goodness of fit test for logistic regression

## correlation matrix (with ARR_DELAY_NEW)
## confusion matrix

set.seed(Sys.time())
test_data = sample_n(Flight_Delays, 1000)
prediction <- predict(log_model, newdata = test_data, type = "response")

#### ROC To Determine Threshold
require(pROC)
roc_log = roc(test_data$ARR_DELAY_NEW, prediction)
plot(roc_log, legacy.axes = TRUE, col=2, main="ROC Curve - Logistic Regression")
threshold = coords(roc_log, "best", "threshold")
print(paste("Probability Threshold: " ,threshold$threshold))

## Confusion Matrix Using Threshold
confusionMatrix(data = as.factor(as.numeric(prediction>threshold$threshold)), reference = test_data$ARR_DELAY_NEW)


#############################################################################################
#############################################################################################

### LINEAR DISCRIMINANT ANALYSIS
require(MASS)
set.seed(Sys.time())
subset = sample_frac(Flight_Delays, size=0.05)
fit_lda = lda(ARR_DELAY_NEW ~ YEAR +QUARTER+MONTH+DAY_OF_WEEK+CARRIER+ORIGIN+DEST+Route+AIR_TIME+DISTANCE , data = subset)

set.seed(Sys.time())
test_data = sample_n(Flight_Delays, 1000)
prediction <- as.numeric(predict(fit_lda, newdata = test_data, type = "response")$class)

require(pROC)
roc_log = roc(test_data$ARR_DELAY_NEW, prediction)
plot(roc_log, legacy.axes = TRUE, col=2, main="ROC Curve - LDA")
threshold = coords(roc_log, "best", "threshold")
print(paste("Probability Threshold: " ,threshold$threshold))

confusionMatrix(data = as.factor(as.numeric(prediction>threshold$threshold)), reference = test_data$ARR_DELAY_NEW)


#############################################################################################
#############################################################################################

## KNN

require(class)
set.seed(Sys.time())
subset = na.omit(sample_frac(Flight_Delays, size=0.05))
test_data = na.omit(sample_frac(Flight_Delays, 0.05))
knn_mod = knn(subset, test_data, cl=subset$ARR_DELAY_NEW,k=5)

#############################################################################################
#############################################################################################


## Fitting Exponential Distribution

hist(Flight_Delays$ARR_DELAY_NEW, xlim=c(0,500), breaks = 300)
#Estimate lambda
n=100
moment_data = rep(0,n)
for (i in 1:n) {
  set.seed(Sys.time())
  subset = sample_frac(Flight_Delays, size=0.05)
  moment_est = 1/mean(subset$ARR_DELAY_NEW, na.rm=T)
  moment_data[i] = moment_est
}
lambda_est = mean(moment_data)


############plot against an exponential distribution


subset = na.omit(subset)
Sdata = matrix(subset$ARR_DELAY_NEW, nrow(subset))

n = nrow(subset)

nqexp = rep(0,n)
for(i in c(1:n)){
  nqexp[i] = qexp((i-0.5)/n, rate=lambda_est)
}
# sort data from smallest to largest
xsort = sort(Sdata)
# Make QQplot
plot(nqexp,xsort,type='p',main="QQplot - Delays vs. Exponential(lambda estimate)",xlab="t-quantiles",
     ylab="quantiles of data",lwd=2)
lines(nqexp,nqexp,lwd=2,col="blue")

#Plot distribution of ARR_DELAY_NEW and the exponential distribution estimated to model it
sorted = as.vector(sort(rexp(nrow(subset), lambda_est), decreasing=T))
exp_test = data.frame(index = 1:nrow(subset),theoretical = sorted, data = as.vector(sort(subset$ARR_DELAY_NEW, decreasing=T)))
ggplot(exp_test) + geom_point(aes(x=index, y=theoretical, color='blue')) + geom_point(aes(x=index, y=data, color='red')) 

#############plot against our mixture distribution

subset = na.omit(FlightDelaysFinal_01)
Sdata = matrix(subset, nrow(subset))

n = nrow(subset)

bernvec <- rbinom(100000,1,.2052)
modelvec <- bernvec*rexp(100000,.0164) + (1-bernvec)*rnorm(100000,-9.005, 11.444)
modelvec <- sort(modelvec)
nqmix = rep(0,n)
for(i in c(1:n)){
  nqmix[i] = quantile(modelvec, probs = (i-.5)/n)
}
percentile <- ecdf(nqmix)

# sort data from smallest to largest
xsort = sort(FlightDelaysFinal_01$ARR_DELAY)
xnew <- percentile(xsort)
# Make QQplot
plot(percentile(nqmix),xnew,type='p',main="QQplot of data vs theoretical mixture distribution",xlab="mixture quantiles",
     ylab="data quantiles",lwd=2)
lines(percentile(nqmix),percentile(nqmix),lwd=2,col="blue")

#Plot distribution of ARR_DELAY_NEW and the exponential distribution estimated to model it
sorted = as.vector(sort(rexp(nrow(subset), lambda_est), decreasing=T))
exp_test = data.frame(index = 1:nrow(subset),theoretical = sorted, data = as.vector(sort(subset$ARR_DELAY_NEW, decreasing=T)))
ggplot(exp_test) + geom_point(aes(x=index, y=theoretical, color='blue')) + geom_point(aes(x=index, y=data, color='red')) 

#######################################################################

#### LOGISTIC REGRESSION WITH WEATHER DATA
FlightDelays05_withWeather$ARR_DELAY_NEW = ifelse(FlightDelays05_withWeather$ARR_DELAY_NEW > 0, 1, FlightDelays05_withWeather$ARR_DELAY_NEW) 
FlightDelays05_withWeather$ARR_DELAY_NEW = as.factor(FlightDelays05_withWeather$ARR_DELAY_NEW)

#SET all NA in wt## to 0 so that it can be made a binary dummy
FlightDelays05_withWeather$wt01[is.na(FlightDelays05_withWeather$wt01)] = 0
FlightDelays05_withWeather$wt02[is.na(FlightDelays05_withWeather$wt02)] = 0
FlightDelays05_withWeather$wt03[is.na(FlightDelays05_withWeather$wt03)] = 0
FlightDelays05_withWeather$wt04[is.na(FlightDelays05_withWeather$wt04)] = 0
FlightDelays05_withWeather$wt05[is.na(FlightDelays05_withWeather$wt05)] = 0
FlightDelays05_withWeather$wt06[is.na(FlightDelays05_withWeather$wt06)] = 0
FlightDelays05_withWeather$wt07[is.na(FlightDelays05_withWeather$wt07)] = 0
FlightDelays05_withWeather$wt08[is.na(FlightDelays05_withWeather$wt08)] = 0
FlightDelays05_withWeather$wt09[is.na(FlightDelays05_withWeather$wt09)] = 0
FlightDelays05_withWeather$wt10[is.na(FlightDelays05_withWeather$wt10)] = 0
FlightDelays05_withWeather$wt11[is.na(FlightDelays05_withWeather$wt11)] = 0

FlightDelays05_withWeather$wt01.DEST[is.na(FlightDelays05_withWeather$wt01.DEST)] = 0
FlightDelays05_withWeather$wt02.DEST[is.na(FlightDelays05_withWeather$wt02.DEST)] = 0
FlightDelays05_withWeather$wt03.DEST[is.na(FlightDelays05_withWeather$wt03.DEST)] = 0
FlightDelays05_withWeather$wt04.DEST[is.na(FlightDelays05_withWeather$wt04.DEST)] = 0
FlightDelays05_withWeather$wt05.DEST[is.na(FlightDelays05_withWeather$wt05.DEST)] = 0
FlightDelays05_withWeather$wt06.DEST[is.na(FlightDelays05_withWeather$wt06.DEST)] = 0
FlightDelays05_withWeather$wt07.DEST[is.na(FlightDelays05_withWeather$wt07.DEST)] = 0
FlightDelays05_withWeather$wt08.DEST[is.na(FlightDelays05_withWeather$wt08.DEST)] = 0
FlightDelays05_withWeather$wt09.DEST[is.na(FlightDelays05_withWeather$wt09.DEST)] = 0
FlightDelays05_withWeather$wt10.DEST[is.na(FlightDelays05_withWeather$wt10.DEST)] = 0
FlightDelays05_withWeather$wt11.DEST[is.na(FlightDelays05_withWeather$wt11.DEST)] = 0

## SET all wt## to factors
FlightDelays05_withWeather$wt01 = as.factor(FlightDelays05_withWeather$wt01)
FlightDelays05_withWeather$wt02 = as.factor(FlightDelays05_withWeather$wt02)
FlightDelays05_withWeather$wt03 = as.factor(FlightDelays05_withWeather$wt03)
FlightDelays05_withWeather$wt04 = as.factor(FlightDelays05_withWeather$wt04)
FlightDelays05_withWeather$wt06 = as.factor(FlightDelays05_withWeather$wt06)
FlightDelays05_withWeather$wt08 = as.factor(FlightDelays05_withWeather$wt08)
FlightDelays05_withWeather$wt09 = as.factor(FlightDelays05_withWeather$wt09)
FlightDelays05_withWeather$wt05 = as.factor(FlightDelays05_withWeather$wt05)
FlightDelays05_withWeather$wt07 = as.factor(FlightDelays05_withWeather$wt07)
FlightDelays05_withWeather$wt10 = as.factor(FlightDelays05_withWeather$wt10)
FlightDelays05_withWeather$wt11 = as.factor(FlightDelays05_withWeather$wt11)

FlightDelays05_withWeather$wt01.DEST = as.factor(FlightDelays05_withWeather$wt01.DEST)
FlightDelays05_withWeather$wt02.DEST = as.factor(FlightDelays05_withWeather$wt02.DEST)
FlightDelays05_withWeather$wt03.DEST = as.factor(FlightDelays05_withWeather$wt03.DEST)
FlightDelays05_withWeather$wt04.DEST = as.factor(FlightDelays05_withWeather$wt04.DEST)
FlightDelays05_withWeather$wt06.DEST = as.factor(FlightDelays05_withWeather$wt06.DEST)
FlightDelays05_withWeather$wt08.DEST = as.factor(FlightDelays05_withWeather$wt08.DEST)
FlightDelays05_withWeather$wt09.DEST = as.factor(FlightDelays05_withWeather$wt09.DEST)
FlightDelays05_withWeather$wt05.DEST = as.factor(FlightDelays05_withWeather$wt05.DEST)
FlightDelays05_withWeather$wt07.DEST = as.factor(FlightDelays05_withWeather$wt07.DEST)
FlightDelays05_withWeather$wt10.DEST = as.factor(FlightDelays05_withWeather$wt10.DEST)
FlightDelays05_withWeather$wt11.DEST = as.factor(FlightDelays05_withWeather$wt11.DEST)

FlightDelays05_withWeather$ARR_DEL15 = as.factor(FlightDelays05_withWeather$ARR_DEL15)

quantile(x = FlightDelays05_withWeather$ARR_DELAY, probs = c(.05, .95))
middle90 = filter(FlightDelays05_withWeather, ARR_DELAY > -26 && ARR_DELAY < 74)

set.seed(Sys.time())
weather_training = sample_frac(FlightDelays05_withWeather, 0.8)
weather_logistic = glm(ARR_DEL15 ~ YEAR + QUARTER + MONTH+ DISTANCE +  CRS_DEP_TIME + CRS_ARR_TIME + prcp+snow+tavg+tmax+tmin+wt01+wt02+wt03+wt04+wt05+wt06+wt07+wt08+wt09+wt10+wt01.DEST+wt02.DEST+wt03.DEST+wt04.DEST+wt05.DEST+wt06.DEST+wt07.DEST+wt08.DEST+wt09.DEST+wt10.DEST+wt11.DEST, data= weather_training, family = "binomial") ##wt11 problematic

summary(weather_logistic)
step_selection = step(weather_logistic, scope=list(upper=~., lower=~1), k=2, direction = "forward") 
summary(step_selection)


require(ResourceSelection)
### Hosmer-Lemeshow goodness of fit test for logistic regression (large p-value means good fit)
#print p-value for different group size 
# small p-value means not a good fit 
for (i in 5:15) {
  print(hoslem.test(weather_logistic$y, fitted(weather_logistic), g=i)$p.value)
}


#Test or model utiltiy:
with(weather_logistic, null.deviance - deviance) #test-statistics
with(weather_logistic, df.null - df.residual) #df for chi-square
with(weather_logistic, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #p-value

set.seed(Sys.time())
weather_testing = sample_frac(FlightDelays05_withWeather, 0.2)
prediction_weather <- predict(weather_logistic, newdata = weather_testing, type = "response")

#### ROC To Determine Threshold
require(pROC)
roc_log_weather = roc(weather_testing$ARR_DEL15, prediction_weather)
plot(roc_log_weather, legacy.axes = TRUE, col=2, main="ROC Curve - Logistic Regression (Weather)")
threshold_weather = coords(roc_log_weather, "best", "threshold")
print(paste("Probability Threshold: " ,threshold_weather$threshold))

## Confusion Matrix Using Threshold
confusionMatrix(data = as.factor(as.numeric(prediction_weather>threshold_weather$threshold)), reference = weather_testing$ARR_DEL15)


############################################
### FULL MODEL (weather & airfare)

FlightDelays_Full$wt01[is.na(FlightDelays_Full$wt01)] = 0
FlightDelays_Full$wt02[is.na(FlightDelays_Full$wt02)] = 0
FlightDelays_Full$wt03[is.na(FlightDelays_Full$wt03)] = 0
FlightDelays_Full$wt04[is.na(FlightDelays_Full$wt04)] = 0
FlightDelays_Full$wt05[is.na(FlightDelays_Full$wt05)] = 0
FlightDelays_Full$wt06[is.na(FlightDelays_Full$wt06)] = 0
FlightDelays_Full$wt07[is.na(FlightDelays_Full$wt07)] = 0
FlightDelays_Full$wt08[is.na(FlightDelays_Full$wt08)] = 0
FlightDelays_Full$wt09[is.na(FlightDelays_Full$wt09)] = 0
FlightDelays_Full$wt10[is.na(FlightDelays_Full$wt10)] = 0
FlightDelays_Full$wt11[is.na(FlightDelays_Full$wt11)] = 0

FlightDelays_Full$wt01.DEST[is.na(FlightDelays_Full$wt01.DEST)] = 0
FlightDelays_Full$wt02.DEST[is.na(FlightDelays_Full$wt02.DEST)] = 0
FlightDelays_Full$wt03.DEST[is.na(FlightDelays_Full$wt03.DEST)] = 0
FlightDelays_Full$wt04.DEST[is.na(FlightDelays_Full$wt04.DEST)] = 0
FlightDelays_Full$wt05.DEST[is.na(FlightDelays_Full$wt05.DEST)] = 0
FlightDelays_Full$wt06.DEST[is.na(FlightDelays_Full$wt06.DEST)] = 0
FlightDelays_Full$wt07.DEST[is.na(FlightDelays_Full$wt07.DEST)] = 0
FlightDelays_Full$wt08.DEST[is.na(FlightDelays_Full$wt08.DEST)] = 0
FlightDelays_Full$wt09.DEST[is.na(FlightDelays_Full$wt09.DEST)] = 0
FlightDelays_Full$wt10.DEST[is.na(FlightDelays_Full$wt10.DEST)] = 0
FlightDelays_Full$wt11.DEST[is.na(FlightDelays_Full$wt11.DEST)] = 0

## SET all wt## to factors
FlightDelays_Full$wt01 = as.factor(FlightDelays_Full$wt01)
FlightDelays_Full$wt02 = as.factor(FlightDelays_Full$wt02)
FlightDelays_Full$wt03 = as.factor(FlightDelays_Full$wt03)
FlightDelays_Full$wt04 = as.factor(FlightDelays_Full$wt04)
FlightDelays_Full$wt06 = as.factor(FlightDelays_Full$wt06)
FlightDelays_Full$wt08 = as.factor(FlightDelays_Full$wt08)
FlightDelays_Full$wt09 = as.factor(FlightDelays_Full$wt09)
FlightDelays_Full$wt05 = as.factor(FlightDelays_Full$wt05)
FlightDelays_Full$wt07 = as.factor(FlightDelays_Full$wt07)
FlightDelays_Full$wt10 = as.factor(FlightDelays_Full$wt10)
FlightDelays_Full$wt11 = as.factor(FlightDelays_Full$wt11)

FlightDelays_Full$wt01.DEST = as.factor(FlightDelays_Full$wt01.DEST)
FlightDelays_Full$wt02.DEST = as.factor(FlightDelays_Full$wt02.DEST)
FlightDelays_Full$wt03.DEST = as.factor(FlightDelays_Full$wt03.DEST)
FlightDelays_Full$wt04.DEST = as.factor(FlightDelays_Full$wt04.DEST)
FlightDelays_Full$wt06.DEST = as.factor(FlightDelays_Full$wt06.DEST)
FlightDelays_Full$wt08.DEST = as.factor(FlightDelays_Full$wt08.DEST)
FlightDelays_Full$wt09.DEST = as.factor(FlightDelays_Full$wt09.DEST)
FlightDelays_Full$wt05.DEST = as.factor(FlightDelays_Full$wt05.DEST)
FlightDelays_Full$wt07.DEST = as.factor(FlightDelays_Full$wt07.DEST)
FlightDelays_Full$wt10.DEST = as.factor(FlightDelays_Full$wt10.DEST)
FlightDelays_Full$wt11.DEST = as.factor(FlightDelays_Full$wt11.DEST)

FlightDelays_Full$ARR_DEL15 = as.factor(FlightDelays_Full$ARR_DEL15)

set.seed(Sys.time())
weather_training_full = sample_frac(FlightDelays_Full, 0.05)
weather_logistic_full = glm(ARR_DEL15 ~ YEAR + QUARTER + MONTH+ DISTANCE +  CRS_DEP_TIME + CRS_ARR_TIME + nsmiles + fare + carrier_lg+large_ms+fare_lg+carrier_low +lf_ms+ prcp+snow+tavg+tmax+tmin+wt01+wt02+wt03+wt04+wt05+wt06+wt07+wt08+wt09+wt10+wt01.DEST+wt02.DEST+wt03.DEST+wt04.DEST+wt05.DEST+wt06.DEST+wt07.DEST+wt08.DEST+wt09.DEST+wt10.DEST+wt11.DEST, data= weather_training_full, family = "binomial") ##wt11 problematic

summary(weather_logistic_full)

set.seed(Sys.time())
weather_testing_full = sample_frac(FlightDelays_Full, 0.05)
prediction_weather_full <- predict(weather_logistic_full, newdata = weather_testing_full, type = "response")

#### ROC To Determine Threshold
require(pROC)
roc_log_weather_full = roc(weather_testing_full$ARR_DEL15, prediction_weather_full)
plot(roc_log_weather_full, legacy.axes = TRUE, col=2, main="ROC Curve - Logistic Regression (Weather & Airfare)")
threshold_weather_full = coords(roc_log_weather_full, "best", "threshold")
print(paste("Probability Threshold: " ,threshold_weather_full$threshold))

## Confusion Matrix Using Threshold
confusionMatrix(data = as.factor(as.numeric(prediction_weather_full>threshold_weather_full$threshold)), reference = weather_testing_full$ARR_DEL15)

## KNN
knn_data = select(FlightDelays_Full, ARR_DELAY, prcp:lf_ms)
knn_sample = sample_n(rf_data, 1000)
set.seed(Sys.time())
data_idx = createDataPartition(knn_sample$ARR_DELAY, p = 0.75, list = FALSE)
data_trn = knn_sample[data_idx, ]
data_tst = knn_sample[-data_idx, ]
knn_mod = train(
  ARR_DELAY ~ .,
  data = data_trn,
  method = "knn",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("impute", "center", "scale"),
  na.fail = na.pass,
  tuneGrid = expand.grid(k = seq(1, 101, by = 2))
)


arr_delay = as.data.frame(group_by(FlightDelays_Full, DEST, QUARTER) %>% mutate(avg_arr_delay = mean(ARR_DELAY)))
arr_delay_subset = sample_n(arr_delay, 10000)
fit = lm(ARR_DELAY ~ avg_arr_delay + DISTANCE, data = arr_delay)
summary(fit)
