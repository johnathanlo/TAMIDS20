FlightDelays <- read.csv("FlightDelays.csv")
FlightDelays$YEAR <- as.factor(FlightDelays$YEAR)
FlightDelays$QUARTER <- as.factor(FlightDelays$QUARTER)
FlightDelays$MONTH <- as.factor(FlightDelays$MONTH)
FlightDelays$DAY_OF_MONTH <- as.factor(FlightDelays$DAY_OF_MONTH)
FlightDelays$DAY_OF_WEEK <- as.factor(FlightDelays$DAY_OF_WEEK)

require(dplyr);require(dummies)
set.seed(0)
FlightDelays05 <- sample_frac(FlightDelays, .05)
FlightDelays05 <- dummy_cols(FlightDelays05)











