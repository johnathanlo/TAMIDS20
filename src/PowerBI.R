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
