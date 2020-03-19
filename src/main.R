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
require(pscl); require(MXM)

######Sample for EDA######
set.seed(0)
FlightDelays05 <- sample_frac(FlightDelays, .05)
FlightDelays05 <- dummy_cols(FlightDelays05)
rm(list = c("FlightDelays"))###save memory
######Sample for EDA######

######Group by Carrier######
FlightDelays05_byCarrier <- group_by(FlightDelays05, CARRIER)
MeanDelaysbyCarrier <- summarise(FlightDelays05_byCarrier, avg = mean(LATE_ARR)) 
######Group by Carrier######



######Histogram of Late Arrivals######
source("src/MakeLateArrHist.R")
LateArrHist <- MakeLateArrHist(FlightDelays05 = FlightDelays05)
plot(LateArrHist)

###For AA###
AA_LateArrs <- MakeLateArrHist(FlightDelays05 = FlightDelays05, carrier = "CARRIER_AA")
plot(AA_LateArrs)

UA_LateArrs <- MakeLateArrHist(FlightDelays05 = FlightDelays05, carrier = "CARRIER_UA")
plot(UA_LateArrs)
######Histogram of Late Arrivals######

######Inflated Poisson Regression######