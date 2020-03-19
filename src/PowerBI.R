
library(ggmap)
library(dplyr)

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
