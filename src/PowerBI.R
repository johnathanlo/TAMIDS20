library(ggplot2)
library(ggmap)
library(dplyr)
require(ggrepel)

##########################################################
#used https://openflights.org/data.html to append long/lat geo coords to each airport for origin and destination airport code to provided Routes.csv (for ggmap functions)

routes = read.csv("data/Routes_Coords.csv")
names(routes)[1]="Index"

key = ""
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
qmplot(Longitude, Latitude, data = avg_delay_by_airport_coords, alpha = Avg.Origin.Delay)+ geom_point() + scale_alpha_continuous(range=c(0.1,1)) + labs(color='NEW LEGEND TITLE')

qmplot(Longitude, Latitude, data = avg_delay_by_airport_coords, alpha = Avg.Dest.Delay)+ geom_point() + scale_alpha_continuous(range=c(0.1,1))   

##Combined Plot of Delays with Airport Popularity
popular_airport_delays = read.csv("data/Airport_Coords_Avg_Delays.csv")
popular_airport_delays = filter(popular_airport_delays, Num_Flights>100000)
names(popular_airport_delays)[5] = "Mean_Destination_Delay_Minutes"
names(popular_airport_delays)[6] = "Total_Number_of_Flights"
##Delay and Popularity (Destination)
qmplot(Longitude, Latitude, data = popular_airport_delays)+ geom_point(aes(alpha=Mean_Destination_Delay_Minutes,size = Total_Number_of_Flights)) + scale_alpha_continuous(range=c(0.1,1)) 

##Delay and Popularity (Origin)
qmplot(Longitude, Latitude, data = popular_airport_delays)+ geom_point(aes(alpha=Avg.Origin.Delay,size = Num_Flights)) + scale_alpha_continuous(range=c(0.1,1)) 


## When Flights Are Scheduled for Days of the Week
plot(FlightDelays$DAY_OF_WEEK ~ FlightDelays$DEP_TIME_BLK)

## how far destination cities are from a given originating airport. This is of importance as longer the flight, airlines can make up time in the air.
plot(FlightDelays$DISTANCE ~ FlightDelays$ORIGIN, xlab="Origin", ylab= "Distance", main = "Flight Distance Available to Make Up Lost Departure Time")

## When are delays most evident for days of week
plot(FlightDelays$ARR_DEL15 ~ FlightDelays$DAY_OF_WEEK)

## Histogram of Delays by Carrier

delays_by_airport = group_by(FlightDelays_Full, CARRIER, DEST) %>% summarise(avg_arr_delay = mean(ARR_DELAY_NEW, na.rm=TRUE))



AA = filter(FlightDelays_Final, CARRIER == "AA")
UA = filter(FlightDelays_Final, CARRIER == "UA")
AS = filter(FlightDelays_Final, CARRIER == "AS")
DL = filter(FlightDelays_Final, CARRIER == "DL")
E9 = filter(FlightDelays_Final, CARRIER == "9E")
B6 = filter(FlightDelays_Final, CARRIER == "B6")
F9 = filter(FlightDelays_Final, CARRIER == "F9")
WN = filter(FlightDelays_Final, CARRIER == "WN")
YV = filter(FlightDelays_Final, CARRIER == "YV")

JFK = filter(FlightDelays_Final, ORIGIN == "JFK") #NYC
LAX = filter(FlightDelays_Final, ORIGIN == "LAX")
DFW = filter(FlightDelays_Final, ORIGIN == "DFW")
ATL = filter(FlightDelays_Final, ORIGIN == "ATL")
ORD = filter(FlightDelays_Final, ORIGIN == "ORD")#Chicago 
DEN = filter(FlightDelays_Final, ORIGIN == "DEN")
SEA = filter(FlightDelays_Final, ORIGIN == "SEA")
MIA = filter(FlightDelays_Final, ORIGIN == "MIA")
DCA = filter(FlightDelays_Final, ORIGIN == "DCA")

Q1 = filter(FlightDelays_Final, QUARTER == "1")
Q2 = filter(FlightDelays_Final, QUARTER == "2")
Q3 = filter(FlightDelays_Final, QUARTER == "3")
Q4 = filter(FlightDelays_Final, QUARTER == "4")

M1 = filter(FlightDelays_Final, MONTH == "1")
M2 = filter(FlightDelays_Final, MONTH == "2")
M3 = filter(FlightDelays_Final, MONTH == "3")
M4 = filter(FlightDelays_Final, MONTH == "4")
M5 = filter(FlightDelays_Final, MONTH == "5")
M6 = filter(FlightDelays_Final, MONTH == "6")
M7 = filter(FlightDelays_Final, MONTH == "7")
M8 = filter(FlightDelays_Final, MONTH == "8")
M9 = filter(FlightDelays_Final, MONTH == "9")
M10 = filter(FlightDelays_Final, MONTH == "10")
M11 = filter(FlightDelays_Final, MONTH == "11")
M12 = filter(FlightDelays_Final, MONTH == "12")

D1 = filter(FlightDelays_Final, DAY_OF_WEEK == "1")
D2 = filter(FlightDelays_Final, DAY_OF_WEEK == "2")
D3 = filter(FlightDelays_Final, DAY_OF_WEEK == "3")
D4 = filter(FlightDelays_Final, DAY_OF_WEEK == "4")
D5 = filter(FlightDelays_Final, DAY_OF_WEEK == "5")
D6 = filter(FlightDelays_Final, DAY_OF_WEEK == "6")
D7 = filter(FlightDelays_Final, DAY_OF_WEEK == "7")

T1 = filter(FlightDelays_Full, DEP_TIME_BLK == "0001-0559")
T2 = filter(FlightDelays_Full, (DEP_TIME_BLK == "0600-0659" | DEP_TIME_BLK =="0700-0759"))
T3 = filter(FlightDelays_Full, (DEP_TIME_BLK == "0800-0859" | DEP_TIME_BLK =="0900-0959"))
T4 = filter(FlightDelays_Full, DEP_TIME_BLK == "1000-1059" | DEP_TIME_BLK =="1100-1159")
T5 = filter(FlightDelays_Full, DEP_TIME_BLK == "1200-1259" | DEP_TIME_BLK =="1300-1359")
T6 = filter(FlightDelays_Full, DEP_TIME_BLK == "1400-1459" | DEP_TIME_BLK =="1500-1559")
T7 = filter(FlightDelays_Full, DEP_TIME_BLK == "1600-1659" | DEP_TIME_BLK =="1700-1759")
T8 = filter(FlightDelays_Full, DEP_TIME_BLK == "1800-1859" | DEP_TIME_BLK =="1900-1959")
T9 = filter(FlightDelays_Full, DEP_TIME_BLK == "2000-2059" | DEP_TIME_BLK =="2100-2159")
T10 = filter(FlightDelays_Full, DEP_TIME_BLK == "2200-2259" | DEP_TIME_BLK =="2300-2359")

Route148 = filter(FlightDelays_Final, Route == 148)
Route900 = filter(FlightDelays_Final, Route == 900)
Route2500 = filter(FlightDelays_Final, Route == 2500)
Route5000 = filter(FlightDelays_Final, Route == 5000)

wt01one = filter(FlightDelays_Full, wt01 == 1)
FlightDelays_Full$wt01[is.na(FlightDelays_Full$wt01)] = 0
wt01zero = filter(FlightDelays_Full, wt01==0)

prcp_little = filter(FlightDelays_Full, prcp < 1000)
prcp_big = filter(FlightDelays_Full, prcp >= 1000)

snow_little = filter(FlightDelays_Full, snow < 100)
snow_big = filter(FlightDelays_Full, snow >= 100)

par(mfrow=c(3,3))
#American Airlines
hist(AA$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of American Airlines", xlab="Arrival Delay (minutes)", ylab="Frequency")
##United 
hist(UA$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of United Airlines", xlab="Arrival Delay (minutes)", ylab="Frequency")
## Alaska Airlines
hist(AS$ARR_DELAY, breaks=750, xlim =c(-200,200), main = "Histogram of Alaska Airlines", xlab="Arrival Delay (minutes)", ylab="Frequency")
## Delta Air
hist(DL$ARR_DELAY, breaks=750, xlim =c(-200,200), main = "Histogram of Delta Air", xlab="Arrival Delay (minutes)", ylab="Frequency")
## Pinnacle Airlines (9E)
hist(E9$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Pinnacle Airlines", xlab="Arrival Delay (minutes)", ylab="Frequency")
## JetBlue Airways
hist(B6$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of JetBlue Airways", xlab="Arrival Delay (minutes)", ylab="Frequency")
## Frontier
hist(F9$ARR_DELAY, breaks=750, xlim =c(-200,200), main = "Histogram of Frontier", xlab="Arrival Delay (minutes)", ylab="Frequency")
## Southwest Airlines
hist(WN$ARR_DELAY, breaks=600, xlim =c(-200,200), main = "Histogram of Southwest Airlines", xlab="Arrival Delay (minutes)", ylab="Frequency")
## Mesa Airlines
hist(YV$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Mesa Airlines", xlab="Arrival Delay (minutes)", ylab="Frequency")


par(mfrow=c(3,3))
# NYC
hist(JFK$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of JFK (NYC)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Los Angeles
hist(LAX$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of LAX (Los Angeles)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Dallas
hist(DFW$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of DFW (Dallas)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Atlanta
hist(ATL$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of ATL (Atlanta)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Chicago
hist(ORD$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of ORD (Chicago)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Denver
hist(DEN$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of DEN (Denver)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Seattle
hist(SEA$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of SEA (Seattle)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Miami
hist(MIA$ARR_DELAY, breaks=750, xlim =c(-200,200), main = "Histogram of MIA (Miami)", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Washington, D.C.
hist(DCA$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of DCA (Washington D.C)", xlab="Arrival Delay (minutes)", ylab="Frequency")

par(mfrow=c(1,4))
# Q1
hist(Q1$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 1", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Q2
hist(Q2$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 2", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Q3
hist(Q3$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 3", xlab="Arrival Delay (minutes)", ylab="Frequency")
# Q4
hist(Q4$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Quarter 4", xlab="Arrival Delay (minutes)", ylab="Frequency")


####### BY MONTH
par(mfrow=c(3,4))
# M1
hist(M1$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of January", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M2
hist(M2$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of February", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M3
hist(M3$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of March", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M4
hist(M4$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of April", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M5
hist(M5$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of May", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M6
hist(M6$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of June", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M7
hist(M7$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of July", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M8
hist(M8$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of August", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M9
hist(M9$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of September", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M10
hist(M10$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of October", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M11
hist(M11$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of November", xlab="Arrival Delay (minutes)", ylab="Frequency")
# M12
hist(M12$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of December", xlab="Arrival Delay (minutes)", ylab="Frequency")


##### BY DAY OF WEEK
par(mfrow=c(2,4))
# D1
hist(D1$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Monday", xlab="Arrival Delay (minutes)", ylab="Frequency")
# D2
hist(D2$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Tuesday", xlab="Arrival Delay (minutes)", ylab="Frequency")
# D3
hist(D3$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Wednesday", xlab="Arrival Delay (minutes)", ylab="Frequency")
# D4
hist(D4$ARR_DELAY, breaks=1250, xlim =c(-200,200), main = "Histogram of Thursday", xlab="Arrival Delay (minutes)", ylab="Frequency")
# D5
hist(D5$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Friday", xlab="Arrival Delay (minutes)", ylab="Frequency")
# D6
hist(D6$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Saturday", xlab="Arrival Delay (minutes)", ylab="Frequency")
# D7
hist(D7$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Sunday", xlab="Arrival Delay (minutes)", ylab="Frequency")


#### BY DEP_TIME_BLK
par(mfrow=c(2,5))
#Midnight-6 AM
hist(T1$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of Midnight-6AM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#6AM-8AM
hist(T2$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 6AM-8AM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#8AM-10AM
hist(T3$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 8AM-10AM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#10AM-Noon
hist(T4$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 10AM-Noon", xlab="Arrival Delay (minutes)", ylab="Frequency")
#12PM-2PM
hist(T5$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 12PM-2PM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#2PM-4PM
hist(T6$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 2PM-4PM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#4PM-6PM
hist(T7$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 4PM-6PM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#6PM-8PM
hist(T8$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 6PM-8PM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#8PM=10PM
hist(T9$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 8PM-10PM", xlab="Arrival Delay (minutes)", ylab="Frequency")
#10PM-Midnight
hist(T10$ARR_DELAY, breaks=1000, xlim =c(-200,200), main = "Histogram of 10PM-Midnight", xlab="Arrival Delay (minutes)", ylab="Frequency")


### BY ROUTE
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
hist(snow_big_nozero$ARR_DELAY, breaks=1000, xlim =c(-200,400), main = "Histogram of Snow: >100 mm (Canceled Flights Removed)", xlab = "snow_more$ARR_DELAY")

