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
