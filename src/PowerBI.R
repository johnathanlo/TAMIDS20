#setwd("~/Data Science Competition 20/data") #Isaac setwd
library(ggmap)
library(dplyr)

##########################################################
#used https://openflights.org/data.html to append long/lat geo coords to each airport for origin and destination airport code to provided Routes.csv (for ggmap functions)

routes = read.csv("Routes_Coords.csv")
names(routes)[1]="Index"

qmap("united states", zoom = 4) + geom_path(aes(x = Longitude, y = Latitude), size = .1, data = dataset, colour="red", lineend = "round")
