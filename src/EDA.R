#setwd("~/Data Science Competition 20/data") #Isaac setwd
require(dplyr); require(ggplot2)
###########################################################

FlightDelays <- read.csv("FlightDelays.csv")
FlightDelays$YEAR <- as.factor(FlightDelays$YEAR)
FlightDelays$QUARTER <- as.factor(FlightDelays$QUARTER)
FlightDelays$MONTH <- as.factor(FlightDelays$MONTH)
FlightDelays$DAY_OF_MONTH <- as.factor(FlightDelays$DAY_OF_MONTH)
FlightDelays$DAY_OF_WEEK <- as.factor(FlightDelays$DAY_OF_WEEK)

summary(FlightDelays)
str(FlightDelays)

#problematic
#FlightDelays_complete = FlightDelays[complete.cases(FlightDelays),] #ARR_DELAY and #ARR_DELAY_NEW become the same
#str(FlightDelays_complete)
