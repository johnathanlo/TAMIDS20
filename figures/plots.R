require(ggplot2)
require(gridExtra)

#### Histogram of All Arrival Delays
gg_late_arrival = ggplot(data=FlightDelays_Final) + geom_histogram(aes(x=ARR_DELAY), bins=100, col="darkblue", fill="lightblue") + xlim(-75,300) + geom_vline(aes(xintercept=mean(ARR_DELAY)), color="blue", linetype="dashed", size=1) + labs(x="Arrival Delay (minutes)", y="Count", title="Histogram of Arrival Delays")
plot(gg_late_arrival)


#### Scatterplot of ARR_DELAY vs temperature (org and dest)
easy_plot = sample_n(FlightDelays_Full, 100000)
gg_delay_temp_origin = ggplot(data = easy_plot) + geom_point(aes(x=tavg, y=ARR_DELAY), col="blue") + labs(x="Average Origin Temperature (tenths of degrees C)", y="Arrival Delay (minutes)", title="Scatterplot of Arrival Delay vs. Average Origin Temperature")
gg_delay_temp_dest = ggplot(data = easy_plot) + geom_point(aes(x=tavg.DEST, y=ARR_DELAY), col="blue") + labs(x="Average Destination Temperature (tenths of degrees C)", y="Arrival Delay (minutes)", title="Scatterplot of Arrival Delay vs. Average Destination Temperature")
grid.arrange(gg_delay_temp_origin, gg_delay_temp_dest, nrow = 1)

#### Scatterplot of ARR_DELAY vs temperature (org and dest)
easy_plot1 = sample_n(FlightDelays_Final, 100000)
gg_delay_prcp_origin = ggplot(data = easy_plot1) + geom_point(aes(x=prcp, y=ARR_DELAY), col="blue") + labs(x="Precipitation (tenths of mm)", y="Arrival Delay (minutes)", title="Scatterplot of Arrival Delay vs. Precipitation (at Origin)")
gg_delay_prcp_dest = ggplot(data = easy_plot1) + geom_point(aes(x=prcp.DEST, y=ARR_DELAY), col="blue") + labs(x="Precipitation (tenths of mm)", y="Arrival Delay (minutes)", title="Scatterplot of Arrival Delay vs. Precipitation (at Destination)")
grid.arrange(gg_delay_prcp_origin, gg_delay_prcp_dest, nrow = 1)


### HISTOGRAMS OF ARR_DELAY for all unusual weather events
WT01 = filter(FlightDelays_Final, wt01==1)
#WT02 = filter(FlightDelays_Final, wt02==1) repetitive of wt01 (to get an clean 10 plots)
WT03 = filter(FlightDelays_Final, wt03==1)
WT04 = filter(FlightDelays_Final, wt04==1)
WT05 = filter(FlightDelays_Final, wt05==1)
WT06 = filter(FlightDelays_Final, wt06==1)
WT07 = filter(FlightDelays_Final, wt07==1)
WT08 = filter(FlightDelays_Final, wt08==1)
WT09 = filter(FlightDelays_Final, wt09==1)
WT10 = filter(FlightDelays_Final, wt10==1)
WT11 = filter(FlightDelays_Final, wt11==1)

par(mfrow=c(2,5))
# WT01
hist(WT01$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Fog, Ice, or Freezing Fog", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT03
hist(WT03$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Thunder", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT04
hist(WT04$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Ice Pellets, Sleet, or Small Hail", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT05
hist(WT05$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Hail", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT06
hist(WT06$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Glaze or Rime", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT07
hist(WT07$ARR_DELAY, breaks=750, xlim =c(-200,200), main = "Histogram of Dust, Ash, or Blowing Obstruction", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT08
hist(WT08$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Smoke or Haze", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT09
hist(WT09$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Blowing or Drifting Snow", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT10
hist(WT10$ARR_DELAY, breaks=500, xlim =c(-200,200), main = "Histogram of Tornado or Water Spout", xlab="Arrival Delay (minutes)", ylab="Frequency")
# WT11
hist(WT11$ARR_DELAY, breaks=500, xlim =c(-200,200), main = "Histogram of High or Damaging Winds", xlab="Arrival Delay (minutes)", ylab="Frequency")


#####################################
## how far destination cities are from a given originating airport. This is of importance as longer the flight, airlines can make up time in the air.
subset = sample_n(FlightDelays_Final, 200000)
plot(subset$DISTANCE ~ subset$ARR_DELAY, xlab="Arrival Delay (minutes)", ylab= "Distance", main = "Scatterplot of Distance vs. Arrival Delay", col="blue")
