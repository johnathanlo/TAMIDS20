###SET WD to Parent folder!###
FlightDelays <- read.csv("data/FlightDelays.csv")
FlightDelays$YEAR <- as.factor(FlightDelays$YEAR)
FlightDelays$QUARTER <- as.factor(FlightDelays$QUARTER)
FlightDelays$MONTH <- as.factor(FlightDelays$MONTH)
FlightDelays$DAY_OF_MONTH <- as.factor(FlightDelays$DAY_OF_MONTH)
FlightDelays$DAY_OF_WEEK <- as.factor(FlightDelays$DAY_OF_WEEK)
FlightDelays$ARR_DELAY[is.na(FlightDelays$ARR_DELAY)]<- 0
FlightDelays$LATE_ARR <- ifelse(FlightDelays$ARR_DELAY>=0, FlightDelays$ARR_DELAY, 0)

require(dplyr);require(dummies); require(fastDummies); require(ggplot2)
set.seed(0)
FlightDelays05 <- sample_frac(FlightDelays, .05)
FlightDelays05 <- dummy_cols(FlightDelays05)

######Histogram of Late Arrivals######
source("src/MakeLateArrHist.R")
LateArrHist <- MakeLateArrHist(FlightDelays05 = FlightDelays05)
plot(LateArrHist)
######Histogram of Late Arrivals######

######Inflated Poisson Regression######