library(ggplot2)
library(ggmap)
library(dplyr)
require(ggrepel)

##########################################################
#used https://openflights.org/data.html to append long/lat geo coords to each airport for origin and destination airport code to provided Routes.csv (for ggmap functions)

routes = read.csv("data/Routes_Coords.csv")
names(routes)[1]="Index"

key = "
"
register_google(key)

## Map of All Possible Flight Routes

routes = read.csv("data/Routes_Coords.csv")
names(routes)[1]="Index"
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = routes, colour="red", lineend = "round")

## Map of All Departure Delayed Flights
delayed_routes_all = read.csv("data/Delayed_Flights_All_Coords.csv")

names(delayed_routes_all)[1]="Index"
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = delayed_routes_all, colour="blue", lineend = "round")

## Map of Q1 Departure Delayed Flights
delayed_routes_Q1 = read.csv("data/Delayed_Flights_Q1_Coords.csv")

names(delayed_routes_Q1)[1]="Index"
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = delayed_routes_Q1, colour="blue", lineend = "round")

## Preparing Data for Mean Delay by Route
avg_arr_delay = FlightDelays %>% 
group_by(Route) %>%
summarise(avg_delay=mean(ARR_DELAY_NEW, na.rm=TRUE)) %>% arrange(avg_delay) %>% filter(avg_delay>=0) 
avg_arr_delay$range = cut(avg_arr_delay$avg_delay, c(0,15,30,45,60,1500))
avg_arr_delay=na.omit(avg_arr_delay)
levels(avg_arr_delay$range)[5] = "60+"
  
write.csv(avg_arr_delay,"C:\\Users\\isaac\\Documents\\GitHub\\TAMIDS20\\data\\avg_arr_delay.csv", row.names = FALSE, col.names=TRUE)

#Map of Routes that Have Average of 0-15 Min Arrival Delay (not including 0)
delay015 = read.csv("data/Maps_Delay_Time/0-15.csv")
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = delay015, colour="blue", lineend = "round")

#Map of Routes that Have Average of 15-30 Min Arrival Delay 
delay1530 = read.csv("data/Maps_Delay_Time/15-30.csv")
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = delay1530, colour="blue", lineend = "round")

#Map of Routes that Have Average of 30-45 Min Arrival Delay 
delay3045 = read.csv("data/Maps_Delay_Time/30-45.csv")
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = delay3045, colour="blue", lineend = "round")

#Map of Routes that Have Average of 45-60 Min Arrival Delay 
delay4560 = read.csv("data/Maps_Delay_Time/45-60.csv")
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = delay4560, colour="blue", lineend = "round")

#Map of Routes that Have Average of 60+ Min Arrival Delay 
delay60plus = read.csv("data/Maps_Delay_Time/60+.csv")
qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = delay60plus, colour="blue", lineend = "round")

#Map of Airports 
airport_coords = read.csv("data/Airport_Coords.csv")
names(airport_coords)[1] = "Airport"
qmplot(Longitude, Latitude, data = airport_coords, colour = I('red'), size = I(1), darken = .3)

#Map of Airports with Usage
num_flights_origin = group_by(FlightDelays, ORIGIN) %>% summarise(num_origin = n())
num_flights_dest = group_by(FlightDelays, DEST) %>% summarise(num_dest = n())
Num_Flights = num_flights_dest$num_dest + num_flights_origin$num_origin
airport = num_flights_dest$DEST
num_flights_airport = data.frame(airport, Num_Flights)
write.csv(num_flights_airport,"C:\\Users\\isaac\\Documents\\GitHub\\TAMIDS20\\data\\num_flights_airport.csv", row.names = FALSE, col.names=TRUE)
num_flights_airport2 = data.frame(Num_Flights)
num_flights_airport_coords = append(airport_coords, num_flights_airport2)
num_flights_airport_coords = as.data.frame(num_flights_airport_coords)

num_flights_under_100000 = filter(num_flights_airport_coords, Num_Flights<100000)
num_flights_over_100000 = filter(num_flights_airport_coords, Num_Flights>100000)

      ## Map of Airports with Less than 100,000 Flights
qmplot(Longitude, Latitude, data = num_flights_under_100000, alpha = Num_Flights)+ geom_point() + scale_alpha_continuous(range=c(0.1,1))

      ## Map of Airports with More than 100,000 Flights
qmplot(Longitude, Latitude, data = num_flights_over_100000, alpha = Num_Flights)+ geom_point() + scale_alpha_continuous(range=c(0.1,1))

## Plot of Departure/Arrival Destination Mean Delay
avg_arr_delay_airport = group_by(FlightDelays, DEST) %>% summarise(mean = mean(ARR_DELAY_NEW, na.rm=TRUE))
avg_dep_delay_airport = group_by(FlightDelays, ORIGIN) %>% summarise(mean = mean(DEP_DELAY_NEW, na.rm=TRUE))
write.csv(avg_arr_delay_airport,"C:\\Users\\isaac\\Documents\\GitHub\\TAMIDS20\\data\\by_airport_arr_delay.csv", row.names = FALSE, col.names=TRUE)
write.csv(avg_dep_delay_airport,"C:\\Users\\isaac\\Documents\\GitHub\\TAMIDS20\\data\\by_airport_dep_delay.csv", row.names = FALSE, col.names=TRUE)

avg_delay_by_airport_coords = read.csv("data/Airport_Coords_Avg_Delays.csv")
qmplot(Longitude, Latitude, data = avg_delay_by_airport_coords, alpha = Avg.Origin.Delay)+ geom_point() + scale_alpha_continuous(range=c(0.1,1))

qmplot(Longitude, Latitude, data = avg_delay_by_airport_coords, alpha = Avg.Dest.Delay)+ geom_point() + scale_alpha_continuous(range=c(0.1,1))   

##Combined Plot of Delays with Airport Popularity
popular_airport_delays = read.csv("data/Airport_Coords_Avg_Delays.csv")
popular_airport_delays = filter(popular_airport_delays, Num_Flights>100000)
##Delay and Popularity (Destination)
qmplot(Longitude, Latitude, data = popular_airport_delays)+ geom_point(aes( alpha=Avg.Dest.Delay,size = Num_Flights)) + scale_alpha_continuous(range=c(0.1,1)) 

##Delay and Popularity (Origin)
qmplot(Longitude, Latitude, data = popular_airport_delays)+ geom_point(aes( alpha=Avg.Origin.Delay,size = Num_Flights)) + scale_alpha_continuous(range=c(0.1,1)) 


## When Flights Are Scheduled for Days of the Week
plot(FlightDelays$DAY_OF_WEEK ~ FlightDelays$DEP_TIME_BLK)

## how far destination cities are from a given originating airport. This is of importance as longer the flight, airlines can make up time in the air.
plot(FlightDelays$DISTANCE ~ FlightDelays$ORIGIN)

## When are delays most evident for days of week
plot(FlightDelays$ARR_DEL15 ~ FlightDelays$DAY_OF_WEEK)

## Histogram of Delays by Carrier

delays_by_airport = group_by(FlightDelays_Full, CARRIER, DEST) %>% summarise(avg_arr_delay = mean(ARR_DELAY_NEW, na.rm=TRUE))



AA = filter(FlightDelays_Full, CARRIER == "AA")
UA = filter(FlightDelays_Full, CARRIER == "UA")
AS = filter(FlightDelays_Full, CARRIER == "AS")

JFK = filter(FlightDelays_Full, ORIGIN == "JFK")
LAX = filter(FlightDelays_Full, ORIGIN == "LAX")
AUS = filter(FlightDelays_Full, ORIGIN == "AUS")

Q1 = filter(FlightDelays_Full, QUARTER == "1")
Q2 = filter(FlightDelays_Full, QUARTER == "2")
Q3 = filter(FlightDelays_Full, QUARTER == "3")
Q4 = filter(FlightDelays_Full, QUARTER == "4")

Route148 = filter(FlightDelays_Full, Route == 148)
Route900 = filter(FlightDelays_Full, Route == 900)
Route2500 = filter(FlightDelays_Full, Route == 2500)
Route5000 = filter(FlightDelays_Full, Route == 5000)

wt01one = filter(FlightDelays_Full, wt01 == 1)
FlightDelays_Full$wt01[is.na(FlightDelays_Full$wt01)] = 0
wt01zero = filter(FlightDelays_Full, wt01==0)

prcp_little = filter(FlightDelays_Full, prcp < 1000)
prcp_big = filter(FlightDelays_Full, prcp >= 1000)

snow_little = filter(FlightDelays_Full, snow < 100)
snow_big = filter(FlightDelays_Full, snow >= 100)

par(mfrow=c(1,3))
#AA ARR_DELAY HIST
hist(AA$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Carrier: AA")
##United ARR_DELAY HIST
hist(UA$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Carrier: UA")
## Alaska Airlines ARR_DELAY HIST
hist(AS$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Carrier: AS")


par(mfrow=c(1,3))
# JFK
hist(JFK$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Airport: JFK (New York)")
# LAX
hist(LAX$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Airport: LAX (California)")
# AUStin
hist(AUS$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Airport: AUS (Texas)")

par(mfrow=c(1,4))
# Q1
hist(Q1$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 1")
# Q2
hist(Q2$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 2")
# Q3
hist(Q3$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 3")
# Q4
hist(Q4$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 4")

par(mfrow=c(1,4))
# Route148
hist(Route148$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Route: 148")
# Route900
hist(Route900$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Route: 900")
# Route2500
hist(Route2500$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Route: 2500")
# Route5000
hist(Route5000$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Route: 5000")

par(mfrow=c(1,2))
# wt01one
hist(wt01one$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Fog: Yes, Present", xlab = "fog$ARR_DELAY")
#wt01zero
hist(wt01zero$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Fog: No, Absent", xlab = "nofog$ARR_DELAY")

par(mfrow=c(1,2))
#prcp less than 10 centimeters
hist(prcp_little$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Precipitation: <10 cm", xlab = "prcp_little$ARR_DELAY")
#prcp more than 10 centimeters
hist(prcp_big$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Precipitation: >10 cm", xlab = "prcp_alot$ARR_DELAY")

par(mfrow=c(1,2))
#snow less than 
hist(snow_little$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Snow: <100 mm", xlab = "snow_less$ARR_DELAY")
#prcp more than 10 centimeters
hist(snow_big$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Snow: >100 mm", xlab = "snow_more$ARR_DELAY")

snow_big_nozero = filter(snow_big, CANCELED !=1)
hist(snow_big_nozero$ARR_DELAY, breaks=1000, xlim =c(-200,400))
