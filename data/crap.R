FlightDelays <- dummy_columns(FlightDelays)
fit <- lm(data = FlightDelays, ARR_DELAY~EMPFULL + DISTANCE)
plot(fit)
summary(fit)
x <- c()
for (i in 1:10000){
  x<- c(x, dweibull(i/100, shape = 2,scale = 2))
}
plot(x, xlim = c(0,1000))
library(dplyr)
sample05<-sample_frac(FlightDelays, .05) 
sample05_dummy<- dummy_columns(sample05)

yearfit <- lm(data = FlightDelays05, FlightDelays05$LATE_ARR ~FlightDelays05$YEAR)
summary(yearfit)
boxplot(log(FlightDelays05$LATE_ARR)~FlightDelays05$YEAR)
anova(yearfit)
hist(log(FlightDelays05$LATE_ARR),breaks = 20)
plot(density(log(FlightDelays05$LATE_ARR)))
hist(FlightDelays05$LATE_ARR, breaks = 100, xlab = "Lateness", main = "Late arrivals")
max(FlightDelays05$LATE_ARR)
quantile(FlightDelays05$LATE_ARR, probs = c(.01, .99))
hist(FlightDelays05$LATE_ARR[FlightDelays05$LATE_ARR<188], breaks = 20, xlab = "Lateness", main = "Late arrivals")

UA <- filter(FlightDelays05_byCarrier, CARRIER =="UA")
hist(UA$LATE_ARR[UA$LATE_ARR<100], breaks = 30)
plot(density(UA$LATE_ARR[UA$LATE_ARR<100]))
exp <- dexp(0:100,rate = 1/24.2897)
plot(exp)
hist(UA$ARR_DELAY[UA$ARR_DELAY<100 & UA$ARR_DELAY>0], breaks = 30)
mean(UA$ARR_DELAY[UA$ARR_DELAY<100 & UA$ARR_DELAY>0])
hist(UA$ARR_DELAY_NEW[UA$ARR_DELAY_NEW<100], breaks = 30)
hist(UA$ARR_DELAY, breaks = 30)
UAplot <- ggplot(data = UA[UA$ARR_DELAY<100 & UA$ARR_DELAY>0,], aes(x = ))
lines(exp)

hist(log(FlightDelays05$ARR_DELAY[FlightDelays05$ARR_DELAY<100]), breaks = 30)

quantile(FlightDelays05$ARR_DELAY, probs= c(.05,.95))

distancefit <- lm(data = FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$DISTANCE)
summary(distancefit)

latedepfit <- lm(data = FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$DEP_DELAY)
summary(latedepfit)

farefit <- lm(data = FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$FARE)
summary(farefit)

incomefit <- lm(data = FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$NET_INCOME)
summary(incomefit)

passengerfit <- lm(data =FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$PASSENGERS + FlightDelays05$WEATHER_DELAY + FlightDelays05$CARRIER_DELAY +FlightDelays05$NAS_DELAY + FlightDelays05$SECURITY_DELAY + FlightDelays05$LATE_AIRCRAFT_DELAY)
summary(passengerfit)

employeesfit <- lm(data = FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$EMPTOTAL)
summary(employeesfit)
RevbyRoute <- group_by(FlightDelays05, )
totalRevbyCarrier <- summarise(FlightDelays05_byCarrier, sum = sum(OP_REVENUES, na.rm = T))

FlightDelays_byRoute <- group_by(FlightDelays05, Route)

MeanDelays_byRoute <- summarise(FlightDelays_byRoute, avg = mean(ARR_DELAY))
hist(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<75], breaks = 100)
shapiro.test(sample(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<75],5000))
mean(FlightDelays05$ARR_DELAY)
hist(MeanDelays_transform[MeanDelays_transform<50], breaks = 30)
shapiro.test(sample(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50])^(1/3),5000))
shapiro.test(sample((FlightDelays05$ARR_DELAY)^(1/3), 5000))

hist((FlightDelays05$ARR_DELAY)^(1/3))
MeanDelays_transform <- ifelse(MeanDelays_byRoute$avg<0, -(abs(MeanDelays_byRoute$avg)^(1/3)),abs(MeanDelays_byRoute$avg)^(1/3)) 
qqnorm(MeanDelays_transform[MeanDelays_transform<50])
qqline(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50])^(1/3))

mean(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50])^(1/3))
sd(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50])^(1/3))
norm_sample <- rnorm(10000, 1.84, .629)
hist(norm_sample, breaks = 100)
hist(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50&MeanDelays_byRoute$avg>0]^(1/3)), breaks = 100)
shapiro.test(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50&MeanDelays_byRoute$avg>0]^(1/3)))
qqnorm(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50&MeanDelays_byRoute$avg>0]^(1/3)))     
qqline(abs(MeanDelays_byRoute$avg[MeanDelays_byRoute$avg<50&MeanDelays_byRoute$avg>0]^(1/3)))             

hist(FlightDelays05$ARR_DELAY[FlightDelays05$ARR_DELAY<300], breaks = 100)

bernvec <- rbinom(10000,1,.347)
modelvec <- bernvec*rexp(10000,.02551029) + (1-bernvec)*rnorm(10000,-12.647, 8.781)
hist(modelvec, breaks = 100)
plot(density(modelvec))

inflatedvec <- rbinom(10000,1,.5)*rexp(10000,.0255)
hist(inflatedvec, breaks = 100)
hist(FlightDelays05$ARR_DELAY_NEW[FlightDelays05$ARR_DELAY_NEW<200], breaks = 100)

weatherfit <- lm(data = FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$WEATHER_DELAY)
summary(weatherfit)

airplanefit <- lm(data =FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$LATE_AIRCRAFT_DELAY )
summary(airplanefit)

WeatherPlanefit <- lm(data = FlightDelays05, FlightDelays05$WEATHER_DELAY~FlightDelays05$LATE_AIRCRAFT_DELAY)
summary(WeatherPlanefit)

cov(FlightDelays05$WEATHER_DELAY, FlightDelays05$LATE_AIRCRAFT_DELAY)
carrierfit <- lm(data = FlightDelays05, FlightDelays05$ARR_DELAY~FlightDelays05$CARRIER_DELAY)
summary(carrierfit)

dDelays <- function(x, para){
  p = para[1]; mu = para[2]; sd = para[3]; rate = para[4];
  if(x>0){
    return(p*dexp(x, rate = rate) + (1-p)*dnorm(x, mean = mu, sd = sd))
  }else{
    return((1-p)*dnorm(x, mean = mu , sd = sd))
  }
}

loglikDelays <- function(x, para){###para = c(p, mu, sd, rate)
  p = para[1]; mu = para[2]; sd = para[3]; rate = para[4];
  likelihoods <-sapply(x,FUN = dDelays, para = para)
  return(-sum(log(likelihoods)))
}

loglikDelays(x = FlightDelays05$ARR_DELAY, para = c(.5, -10, 5, 1/24))

MLE <- optim(c(.5, -10, 5, 1/24), loglikDelays, x= FlightDelays05$ARR_DELAY) 
###get fisher information matrix
MLE <- c(.3434697830, -12.64704148,   8.78079846 ,  0.02551029)
MLEnew <- MLE$par
bernvec <- rbinom(100000,1,.2052)
modelvec <- bernvec*rexp(100000,.0164) + (1-bernvec)*rnorm(100000,-9.005, 11.444)
par(mfrow = c(2,1))
hist(modelvec, breaks = 100)
hist(FlightDelays05$ARR_DELAY[FlightDelays05$ARR_DELAY<600], breaks = 100)
plot(density(modelvec))

#####inflated exp#####
dDelays2 <- function(x, para){
  p = para[1]; rate = para[2];
  if(x == 0){
    return((1-p) + p*(1-exp(-rate/2)))
  }else{
    return(p*exp(-rate/2))
  }
}

loglikDelays2 <- function(x, para){
  p = para[1]; rate = para[2];
  likelihoods<- sapply(x, FUN = dDelays2, para = para)
  return(-sum(log(likelihoods)))
}
FlightDelays05noNA <- na.omit(FlightDelays05$ARR_DELAY_NEW)
MLE2 <- optim(c(.5,1/24), loglikDelays2, x= FlightDelays05noNA)

MLE2vec <- rbinom(2500000,1,.3879)*rexp(2500000,.1833331)
par(mfrow = c(2,1))
hist(MLE2vec, breaks = 100)
hist(FlightDelays05noNA[FlightDelays05noNA<80], breaks = 100)

par(mfrow = c(2,1))
plot(density(MLE2vec))
plot(density(FlightDelays05noNA[FlightDelays05noNA<80]))
library(rnoaa)
token = "ECJpfJUthLGURRFjeWcFPUThEhVqtbiH"
AirportCoords <- read.csv("data/Airport_Coords.csv")
names(AirportCoords)[1] = "id"
AirportCoords <- na.omit(AirportCoords)
test <- ncdc()

#########RNOAA#########
ncdc_datatypes(token = token)
meteo_stations <- meteo_nearby_stations(AirportCoords, lat_colname = "Latitude", lon_colname = "Longitude")
meteo_stations_id <- c()
for(i in 1:length(meteo_stations)){
  meteo_stations_id <-c(meteo_stations_id, meteo_stations[[i]]$id[1])
}
meteo_stations_name <- c()
for(i in 1:length(meteo_stations)){
  meteo_stations_name <- c(meteo_stations_name, meteo_stations[[i]]$name[1])
}
meteo_stations_dist <- c()
for(i in 1:length(meteo_stations)){
  meteo_stations_dist <- c(meteo_stations_dist, meteo_stations[[i]]$distance[1])
}
meteo_stations_trimmed <- data.frame(AIRPORT = AirportCoords$id, ID = meteo_stations_id, NAME = meteo_stations_name, DIST = meteo_stations_dist)
save(list = c("meteo_stations_trimmed"), file = "data/meteo_stations_trimmed.RData")

weatherdata <- meteo_pull_monitors(monitors = meteo_stations_trimmed$ID, date_min = "2018-01-01", date_max = "2019-12-31", var = c("PRCP", "TAVG", "TMAX", "TMIN", "SNOW", "WSFG", "WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT07", "WT08", "WT09", "WT10", "WT11", "WT12", "WT13", "WT14", "WT15", "WT16", "WT17", "WT18", "WT19", "WT20", "WT21", "WT22"))


#######Redo with current weather stations#######
load("data/stations.RData")
good_stations <- stations$id
meteo_stations_id <- c()
meteo_stations_index <- c()
for(i in 1:length(meteo_stations)){
  notfound = 1
  j = 1
  while(notfound){
    if(meteo_stations[[i]]$id[j] %in% good_stations){
      meteo_stations_id <-c(meteo_stations_id, meteo_stations[[i]]$id[j])
      meteo_stations_index <- c(meteo_stations_index, j)
      notfound = 0
    }else{
      j = j+1
    }
  }
}
meteo_stations_name <- c()
for(i in 1:length(meteo_stations)){
  meteo_stations_name <- c(meteo_stations_name, meteo_stations[[i]]$name[meteo_stations_index[i]])
}
meteo_stations_dist <- c()
for(i in 1:length(meteo_stations)){
  meteo_stations_dist <- c(meteo_stations_dist, meteo_stations[[i]]$distance[meteo_stations_index[i]])
}
meteo_stations_trimmed2 <- data.frame(AIRPORT = AirportCoords$id, ID = meteo_stations_id, NAME = meteo_stations_name, DIST = meteo_stations_dist)
save(list = c("meteo_stations_trimmed2"), file = "data/meteo_stations_trimmed2.RData")
weatherdata2 <- meteo_pull_monitors(monitors = meteo_stations_trimmed2$ID, date_min = "2018-01-01", date_max = "2019-12-31", var = c("PRCP", "TAVG", "TMAX", "TMIN", "SNOW", "WSFG", "WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT07", "WT08", "WT09", "WT10", "WT11", "WT12", "WT13", "WT14", "WT15", "WT16", "WT17", "WT18", "WT19", "WT20", "WT21", "WT22"))
save(list = c("weatherdata2"), file = "data/weatherdata.RData")

names(weatherdata2)[1] = "ID"
merged_weather <- merge(meteo_stations_trimmed2, weatherdata2, by = "ID", all.x = F, all.y = F)
save(list = c("merged_weather"), file = "data/merged_weather.RData")

merged_weather <- as.data.frame(merged_weather)
merged_weather$ORIGIN <- merged_weather$AIRPORT
names(merged_weather)[5] <- "FL_DATE"

FlightDelays0505 <- sample_frac(tbl = FlightDelays05, size = .005)
FlightDelays0505$FL_DATE <- as.factor(FlightDelays0505$FL_DATE)
merged_weather$FL_DATE <- as.factor(merged_weather$FL_DATE)
merged_FlightDelays05 <- merge(FlightDelays0505, merged_weather)

FlightDelays05$FL_DATE <- as.factor(FlightDelays05$FL_DATE)
merged_weather$FL_DATE <- as.factor(merged_weather$FL_DATE)
merged_FlightDelays05 <- merge(FlightDelays05, merged_weather)


merged_weather.dest <- merged_weather
for(i in 1:length(names(merged_weather.dest))){
  names(merged_weather.dest)[i] <- paste(names(merged_weather.dest[i]), ".DEST", sep = "")
}
names(merged_weather.dest)[2] = "DEST"
names(merged_weather.dest)[5] = "FL_DATE"

merged_FlightDelays05 <- merge(merged_FlightDelays05, merged_weather.dest)

FlightDelays05_withWeather <- merged_FlightDelays05
save(list = c("merged_weather.dest"), file = "data/merged_weather_dest.Rdata")
save(list = c("FlightDelays05_withWeather"), file = "data/FlightDelays05_withWeather.RData")

###regress weather
library(caret)
weatherfit <- lm(data = FlightDelays05_withWeather, WEATHER_DELAY~tmax + tmin + tmax.DEST + tmin.DEST + prcp + snow + prcp.DEST + snow.DEST + I(tmax^3) + I(tmin^3) + I(log(prcp)) + I(log(snow)))
weatherfit2 <- glm(ARR_DEL15~ tmax+ tmin + prcp + snow, data = FlightDelays05_withWeather, family = "binomial")
summary(weatherfit)
summary(weatherfit2)
weatherfit2_predict <- predict(weatherfit2, newdata = FlightDelays05_withWeather)
confusionMatrix(data = factor(weatherfit2_predict), reference = factor(FlightDelays05$ARR_DEL15))
plot(weatherfit)
weatherfit2_predict

save(list = c("FlightDelays"), file = "data/FlightDelays.RData")

FlightDelays_Weather <- merge(FlightDelays, merged_weather)
FlightDelays_weatherFull <- merge(FlightDelays_Weather, merged_weather.dest)
save(list = c("FlightDelays_weatherFull"), file = "data/FlightDelays_weatherFull.RData")
AirFare <- read.csv("data/AirFares.csv")
names(AirFare)[2] = "QUARTER"
names(AirFare)[3] = "ORIGIN"
names(AirFare)[5] = "DEST"
names(AirFare)[1] = "YEAR"
FlightDelays_Full<- merge(FlightDelays_weatherFull, AirFare)

save(list = c("FlightDelays_Full"), file = "data/FlightDelays_Full.RData")

####################RF
library(caret)
# library(mice)
# install.packages("VIM")
# library(VIM)
# mice_plot <- aggr(FlightDelays05_withWeather, col=c('navyblue','yellow'),
#                     numbers=TRUE, sortVars=TRUE,
#                     labels=names(iris.mis), cex.axis=.7,
#                     gap=3, ylab=c("Missing data","Pattern"))
# FlightDelays_mice <- mice(data = Flight_trimmed, method = "pmm", seed = 0)
FlightDelays05_withWeather[is.na(FlightDelays05_withWeather)]<- 0
FlightDelays0505 <- sample_frac(FlightDelays05_withWeather, size = .01)
sim_rf_mod = train(
  ARR_DEL15 ~ .,
  data = FlightDelays0505,
  method = "rf",
  trControl = trainControl(method = "cv",number=5),
  #  preProcess = c("center", "scale"),
  tuneLength = 5,
  na.action = na.omit,
)
print(sim_rf_mod)
calc_rmse(actual = sim_tst$y,
          predicted = predict(sim_rf_mod, sim_tst))

library(dplyr)
FlightDelaysFull_LateDeparturesbyAirport <- group_by(FlightDelays_Full, ORIGIN)
FlightDelaysFull_LateDeparturesbyAirport_summary <- summarise(FlightDelaysFull_LateDeparturesbyAirport, mean = mean(DEP_DELAY_NEW))

