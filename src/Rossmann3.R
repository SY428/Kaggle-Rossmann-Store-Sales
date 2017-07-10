### Rossmann Store Sales
setwd("/Users/sophieyang/Documents/Kaggle/Rossmann")

# Set seed
set.seed(1)

# Load Libraries
library(lattice)
library(ggplot2)
library(caret)
library(plyr)
library(lubridate)
library(xgboost)
library(stats)
library(tseries)
library(gridExtra)
library(grid)
library(astsa)
library(stats)

# Get files
train <- read.csv("train.csv")
test <- read.csv("test.csv")
store <- read.csv("store.csv")

# Join the information from training and test set with info from store
trainStore <- join(train, store, by = "Store")
testStore <- join(test, store, by = "Store")

# Date ranges
summary(as.Date(trainStore$Date))
summary(as.Date(testStore$Date))
####################################################################################
### Exploratory Analysis
### 1. Remove outliers
### 2. Reassign NAs
####################################################################################

# Convert to Time Series
trainStore$Date <- as.Date(trainStore$Date)
trainStore$DayOfWeek <- as.character(trainStore$DayOfWeek)
trainStore$DayOfWeek[which(trainStore$DayOfWeek == "1")] <- "Monday"
trainStore$DayOfWeek[which(trainStore$DayOfWeek == "2")] <- "Tuesday"
trainStore$DayOfWeek[which(trainStore$DayOfWeek == "3")] <- "Wednesday"
trainStore$DayOfWeek[which(trainStore$DayOfWeek == "4")] <- "Thursday"
trainStore$DayOfWeek[which(trainStore$DayOfWeek == "5")] <- "Friday"
trainStore$DayOfWeek[which(trainStore$DayOfWeek == "6")] <- "Saturday"
trainStore$DayOfWeek[which(trainStore$DayOfWeek == "7")] <- "Sunday"

trainStore$DayOfWeek <- as.factor(trainStore$DayOfWeek)
trainStore$Open <- as.factor(trainStore$Open)
trainStore$Promo <- as.factor(trainStore$Promo)
trainStore$SchoolHoliday <- as.factor(trainStore$SchoolHoliday)
trainStore$Promo2 <- as.factor(trainStore$Promo2)

# Remove all closed stores
# trainStore <- subset(trainStore, trainStore$Open == 1)

# Check all NAs in 
apply(trainStore,2,function(x) sum(is.na(x)))

StoreSaleMean <- aggregate(Sales~Store, data = trainStore, FUN = mean)
StoreSaleMedian <- aggregate(Sales~Store, data = trainStore, FUN = median)

####################################################
##### Take Store 1
####################################################
trainStore1 <- subset(trainStore, trainStore$Store == 1)
# Plot Sales as a function of Date
ggplot(data = trainStore1, aes(Date, Sales)) + geom_line()
plot2013 <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2013-01-01")) & (trainStore1$Date < as.Date("2014-01-01"))), aes(Date, Sales)) + geom_line()
plot2014 <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2014-01-01")) & (trainStore1$Date < as.Date("2015-01-01"))), aes(Date, Sales)) + geom_line()
grid.arrange(plot2013, plot2014, nrow = 2)
# There is an obvious time trend

# Time series analysis
Store1.diff <- diff(trainStore1$Sales)
adf.test(trainStore1$Sales, alternative="stationary", k=0)
adf.test(trainStore1$Sales, alternative="stationary")
adf.test(trainStore1$Sales, alternative="explosive", k=0)
adf.test(trainStore1$Sales, alternative="explosive")

acf2(trainStore1$Sales)
acf2(Store1.diff)

# Examine seasonal data
Sales12 <- diff(trainStore1$Sales, 12)
plot(Sales12)
acf2(Sales12)

m <- 365
y1 <- ts(trainStore1$Sales, frequency = m)
myARIMAfourier <- Arima(y1, order=c(2,0,1), xreg = fourier(y1, K = 100))
plot(forecast(myARIMAfourier, h = 2*m, xreg = fourier(y1, K = 100, h = 2*m)))

pred.date <- as.Date(unique(testStore$Date))
ggplot(data = subset(trainStore1, trainStore1$Date > as.Date("2015-07-01")), aes(Date, Sales)) + geom_line()

myARIMA.501x011.12 <- arima(trainStore1$Sales, order = c(5,0,1), seasonal = list(order = c(0,1,1), period = 12))
trainSamplePred <- predict(myARIMA.501x011.12, n.ahead = length(pred.date))
plot2013_range <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2013-08-01")) & (trainStore1$Date < as.Date("2013-09-18"))), aes(Date, Sales)) + geom_line()
plot2014_range <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2014-08-01")) & (trainStore1$Date < as.Date("2014-09-18"))), aes(Date, Sales)) + geom_line()
plot2015_range <- ggplot(data = data.frame(Date = as.Date(pred.date), Sales = trainSamplePred$pred), aes(x = Date, y = Sales)) + geom_line()
grid.arrange(plot2013_range, plot2014_range, plot2015_range, nrow = 3)

myARIMA.501x111.12 <- arima(trainStore1$Sales, order = c(5,0,1), seasonal = list(order = c(1,1,1), period = 12))
trainSamplePred <- predict(myARIMA.501x111.12, n.ahead = length(pred.date))
ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2013-08-01")) & (trainStore1$Date < as.Date("2013-09-18"))), aes(Date, Sales)) + geom_line()
ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2014-08-01")) & (trainStore1$Date < as.Date("2014-09-18"))), aes(Date, Sales)) + geom_line()
ggplot(data = data.frame(Date = as.Date(pred.date), Sales = trainSamplePred$pred), aes(Date, Sales)) + geom_line()

myARIMA.min <- arima(trainStore1$Sales, order = c(0,0,0), seasonal = list(order = c(0,0,0), period = 12))
minAIC <- myARIMA.min$aic

for (i in 1:6) { for (j in 0:1) { for (k in 0:5) {
  print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(0,0,0)", sep = ""))
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    myARIMA.temp <- arima(trainStore1$Sales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12))
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("ARIMA: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# arima(trainStore1$Sales, order = c(1,1,5), seasonal = list(order = c(2,0,1), period = 12))
# "ARIMA: (1,1,5),(0,1,3)"
myARIMA.min <- arima(trainStore1$Sales, order = c(1,1,5), seasonal = list(order = c(2,1,1), period = 12))
minAIC <- myARIMA.min$aic

for (i in 2:6) { for (j in 0:1) { for (k in 0:5) {
  for (l in 0:2) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- arima(trainStore1$Sales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12))
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("ARIMA: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

###########################################################
### Add in Exonerous variables
###########################################################
apply(trainStore1,2,function(x) sum(is.na(x)))

xreg1 <- model.matrix(~ droplevels(as.factor(trainStore1$DayOfWeek)) 
                      + trainStore1$Customers
                      + trainStore1$Promo
                      + trainStore1$SchoolHoliday)
xreg1 <- xreg1[,-1]
colnames(xreg1) <- c("Mon", "Sat", "Thur", "Tues", "Wed", "Customers", "Promo", "SchoolHoliday")

Arima(trainStore1$Sales, order = c(1,0,1), seasonal = list(order = c(0,0,1), period = 12))
# sigma^2 estimated as 530142:  log likelihood=-6253.79 AIC=12517.58   AICc=12517.66   BIC=12540.89
Arima(trainStore1$Sales, order = c(1,0,1), seasonal = list(order = c(0,0,1), period = 12), xreg = xreg1)
# sigma^2 estimated as 64764:  log likelihood=-5428.45 AIC=10882.89   AICc=10883.37   BIC=10943.48

myARIMA.min <- Arima(trainStore1$Sales, order = c(0,0,0), seasonal = list(order = c(0,0,0), period = 12), xreg = xreg1)
minAIC <- myARIMA.min$aic

for (i in 0:6) { for (j in 0:1) { for (k in 0:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStore1$Sales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }




###########################################################
##### Take Store 817
###########################################################
trainStore817 <- subset(trainStore, trainStore$Store == 817)
# Plot Sales as a function of Date
ggplot(data = trainStore817, aes(Date, Sales)) + geom_line()
plot2013 <- ggplot(data = subset(trainStore817, (trainStore817$Date >= as.Date("2013-01-01")) & (trainStore817$Date < as.Date("2014-01-01"))), aes(Date, Sales)) + geom_line()
plot2014 <- ggplot(data = subset(trainStore817, (trainStore817$Date >= as.Date("2014-01-01")) & (trainStore817$Date < as.Date("2015-01-01"))), aes(Date, Sales)) + geom_line()
grid.arrange(plot2013, plot2014, nrow = 2)
# There is an obvious time trend

# Time series analysis
Store817.diff <- diff(trainStore817$Sales)
adf.test(trainStore817$Sales, alternative="stationary", k=0)
adf.test(trainStore817$Sales, alternative="stationary")
adf.test(trainStore817$Sales, alternative="explosive", k=0)
adf.test(trainStore817$Sales, alternative="explosive")

acf2(trainStore817$Sales)
acf2(Store817.diff)

# Examine seasonal data
Sales12 <- diff(trainStore817$Sales, 12)
plot(Sales12)
acf2(Sales12)

# ARIMA(0,0,1)x(0,0,1).12
arima(trainStore817$Sales, order = c(0,0,1), seasonal = list(order = c(0,0,1), period = 12))
# sigma^2 estimated as 15623390:  log likelihood = -7607.08,  aic = 15222.15

# ARIMA(0,0,1)x(0,1,1).12
arima(trainStore817$Sales, order = c(0,0,1), seasonal = list(order = c(0,1,1), period = 12))
# sigma^2 estimated as 14693471:  log likelihood = -7467.03,  aic = 14940.07

# ARIMA(5,0,1)x(0,1,1).12
arima(trainStore817$Sales, order = c(5,0,1), seasonal = list(order = c(0,1,1), period = 12))
# sigma^2 estimated as 14062029:  log likelihood = -7450.1,  aic = 14916.2

# ARIMA(1,0,0)x(1,0,0).12
arima(trainStore817$Sales, order = c(1,0,0), seasonal = list(order = c(1,0,0), period = 12))
# sigma^2 estimated as 13529172:  log likelihood = -7551.8,  aic = 15111.59

# ARIMA(1,0,0)x(1,0,1).12
arima(trainStore817$Sales, order = c(1,0,0), seasonal = list(order = c(1,0,1), period = 12))
# sigma^2 estimated as 13238460:  log likelihood = -7543.6,  aic = 15097.21

# ARIMA(1,0,1)x(1,0,0).12
arima(trainStore817$Sales, order = c(1,0,1), seasonal = list(order = c(1,0,0), period = 12))
# sigma^2 estimated as 13189211:  log likelihood = -7541.92,  aic = 15093.84

# ARIMA(1,0,1)x(1,0,1).12
arima(trainStore817$Sales, order = c(1,0,1), seasonal = list(order = c(1,0,1), period = 12))
# sigma^2 estimated as 12939817:  log likelihood = -7534.72,  aic = 15081.44

# ARIMA(1,1,1)x(1,0,1).12
arima(trainStore817$Sales, order = c(1,1,1), seasonal = list(order = c(1,0,1), period = 12))
# sigma^2 estimated as 13239936:  log likelihood = -7534.9,  aic = 15079.81

# ARIMA(2,1,1)x(2,1,3).12
arima(trainStore817$Sales, order = c(2,1,1), seasonal = list(order = c(2,1,3), period = 12.1))

pred.date <- as.Date(unique(testStore$Date))
ggplot(data = subset(trainStore817, trainStore817$Date > as.Date("2015-07-01")), aes(Date, Sales)) + geom_line()

myARIMA.211x213.12 <- arima(trainStore817$Sales, order = c(2,1,1), seasonal = list(order = c(2,1,3), period = 12.3))
trainSamplePred <- predict(myARIMA.501x011.12, n.ahead = length(pred.date))
plot2013_range <- ggplot(data = subset(trainStore817, (trainStore817$Date >= as.Date("2013-08-01")) & (trainStore817$Date < as.Date("2013-09-18"))), aes(Date, Sales)) + geom_line()
plot2014_range <- ggplot(data = subset(trainStore817, (trainStore817$Date >= as.Date("2014-08-01")) & (trainStore817$Date < as.Date("2014-09-18"))), aes(Date, Sales)) + geom_line()
plot2015_range <- ggplot(data = data.frame(Date = as.Date(pred.date), Sales = trainSamplePred$pred), aes(x = Date, y = Sales)) + geom_line()
grid.arrange(plot2013_range, plot2014_range, plot2015_range, nrow = 3)


####################################################################################
### Split training data into different groups and train each subgroup separately
####################################################################################

# StoreSaleMean <- aggregate(Sales~Store, data = trainStore, FUN = mean)
# StoreSaleMedian <- aggregate(Sales~Store, data = trainStore, FUN = median)

# Work with median sale price, it's not as skewed








####################################################################################
### Creating New features from existing features