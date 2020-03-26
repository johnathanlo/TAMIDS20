require(caret)
require(dplyr)
require(aod)

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

require(ResourceSelection)
### Hosmer-Lemeshow goodness of fit test for logistic regression
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


