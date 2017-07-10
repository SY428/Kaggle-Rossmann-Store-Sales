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
library(forecast)

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

# New Features

# Was Closed Yesterday
trainStore$ClosedDayPrior <- 0
for (i in 1:length(unique(trainStore$Store))) {
  trainStore_i <- subset(trainStore, trainStore$Store == i)
  # Assume first one open, switch for test case
  trainStore_i$ClosedDayPrior[2:nrow(trainStore_i)] <- as.numeric(!as.numeric(as.character(trainStore_i$Open[1:(nrow(trainStore_i)-1)])))
  trainStore$ClosedDayPrior[which(trainStore$Store == i)] <- trainStore_i$ClosedDayPrior
}
trainStore$ClosedDayPrior <- as.factor(trainStore$ClosedDayPrior)
  
# Will Close Tomorrow
trainStore$ClosingNextDay <- 0
for (i in 1:length(unique(trainStore$Store))) {
  trainStore_i <- subset(trainStore, trainStore$Store == i)
  # Assume last one open, switch for test case
  trainStore_i$ClosingNextDay[1:(nrow(trainStore_i)-1)] <- as.numeric(!as.numeric(as.character(trainStore_i$Open[2:nrow(trainStore_i)])))
  trainStore$ClosingNextDay[which(trainStore$Store == i)] <- trainStore_i$ClosingNextDay
}
trainStore$ClosingNextDay <- as.factor(trainStore$ClosingNextDay)

# Will Have Promo in less than 3 days and not promo day
trainStore$PromoWithin3Days <- 0
for (i in 1:length(unique(trainStore$Store))) {
  trainStore_i <- subset(trainStore, trainStore$Store == i)
  # Assume last one open, switch for test case
  trainStore_i$PromoWithin3Days[1:(nrow(trainStore_i)-1)] <- as.numeric(as.character(trainStore_i$Promo[2:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$Promo[1:(nrow(trainStore_i)-1)]))
  trainStore_i$PromoWithin3Days[1:(nrow(trainStore_i)-2)] <- ifelse(trainStore_i$PromoWithin3Days[1:(nrow(trainStore_i)-2)] > 0, 
                                                                    trainStore_i$PromoWithin3Days[1:(nrow(trainStore_i)-2)], 
                                                                    as.numeric(as.character(trainStore_i$Promo[3:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$Promo[1:(nrow(trainStore_i)-2)])))
  trainStore_i$PromoWithin3Days[1:(nrow(trainStore_i)-3)] <- ifelse(trainStore_i$PromoWithin3Days[1:(nrow(trainStore_i)-3)] > 0, 
                                                                    trainStore_i$PromoWithin3Days[1:(nrow(trainStore_i)-3)], 
                                                                    as.numeric(as.character(trainStore_i$Promo[4:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$Promo[1:(nrow(trainStore_i)-3)])))
  trainStore$PromoWithin3Days[which(trainStore$Store == i)] <- trainStore_i$PromoWithin3Days
}
trainStore$PromoWithin3Days <- as.factor(trainStore$PromoWithin3Days)

# Promo Ended less than 3 days ago
trainStore$PromoEndedWithin3Days <- 0
for (i in 1:length(unique(trainStore$Store))) {
  trainStore_i <- subset(trainStore, trainStore$Store == i)
  # Assume first one open, switch for test case
  trainStore_i$PromoEndedWithin3Days[2:nrow(trainStore_i)] <- as.numeric(as.character(trainStore_i$Promo[1:(nrow(trainStore_i)-1)])) & !as.numeric(as.character(trainStore_i$Promo[2:nrow(trainStore_i)]))
  trainStore_i$PromoEndedWithin3Days[3:nrow(trainStore_i)] <- ifelse(trainStore_i$PromoEndedWithin3Days[3:nrow(trainStore_i)] > 0, 
                                                                     trainStore_i$PromoEndedWithin3Days[3:nrow(trainStore_i)], 
                                                                     as.numeric(as.character(trainStore_i$Promo[1:(nrow(trainStore_i)-2)])) & !as.numeric(as.character(trainStore_i$Promo[3:nrow(trainStore_i)])))
  trainStore_i$PromoEndedWithin3Days[4:nrow(trainStore_i)] <- ifelse(trainStore_i$PromoEndedWithin3Days[4:nrow(trainStore_i)] > 0, 
                                                                     trainStore_i$PromoEndedWithin3Days[4:nrow(trainStore_i)], 
                                                                     as.numeric(as.character(trainStore_i$Promo[1:(nrow(trainStore_i)-3)])) & !as.numeric(as.character(trainStore_i$Promo[4:nrow(trainStore_i)])))
  
  
  trainStore$PromoEndedWithin3Days[which(trainStore$Store == i)] <- trainStore_i$PromoEndedWithin3Days
}
trainStore$PromoEndedWithin3Days <- as.factor(trainStore$PromoEndedWithin3Days)

# Was school holiday within 5 days
trainStore$TimePriorWasSchoolHoliday <- 0
for (i in 1:length(unique(trainStore$Store))) {
  trainStore_i <- subset(trainStore, trainStore$Store == i)
  # Assume first one open, switch for test case
  trainStore_i$TimePriorWasSchoolHoliday[2:nrow(trainStore_i)] <- as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-1)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[2:nrow(trainStore_i)]))
  trainStore_i$TimePriorWasSchoolHoliday[3:nrow(trainStore_i)] <- ifelse(trainStore_i$TimePriorWasSchoolHoliday[3:nrow(trainStore_i)] > 0, 
                                                                         trainStore_i$TimePriorWasSchoolHoliday[3:nrow(trainStore_i)], 
                                                                         as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-2)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[3:nrow(trainStore_i)])))
  trainStore_i$TimePriorWasSchoolHoliday[4:nrow(trainStore_i)] <- ifelse(trainStore_i$TimePriorWasSchoolHoliday[4:nrow(trainStore_i)] > 0, 
                                                                         trainStore_i$TimePriorWasSchoolHoliday[4:nrow(trainStore_i)], 
                                                                         as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-3)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[4:nrow(trainStore_i)])))
  trainStore_i$TimePriorWasSchoolHoliday[5:nrow(trainStore_i)] <- ifelse(trainStore_i$TimePriorWasSchoolHoliday[5:nrow(trainStore_i)] > 0, 
                                                                         trainStore_i$TimePriorWasSchoolHoliday[5:nrow(trainStore_i)], 
                                                                         as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-4)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[5:nrow(trainStore_i)])))
  trainStore_i$TimePriorWasSchoolHoliday[6:nrow(trainStore_i)] <- ifelse(trainStore_i$TimePriorWasSchoolHoliday[6:nrow(trainStore_i)] > 0, 
                                                                         trainStore_i$TimePriorWasSchoolHoliday[6:nrow(trainStore_i)], 
                                                                         as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-5)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[6:nrow(trainStore_i)])))
  trainStore$TimePriorWasSchoolHoliday[which(trainStore$Store == i)] <- trainStore_i$TimePriorWasSchoolHoliday
}
trainStore$TimePriorWasSchoolHoliday <- as.factor(trainStore$TimePriorWasSchoolHoliday)

# Going to be school holiday within 5 days
trainStore$DaysWithinSchoolHoliday <- 0
for (i in 1:length(unique(trainStore$Store))) {
  trainStore_i <- subset(trainStore, trainStore$Store == i)
  # Assume last one open, switch for test case
  trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-1)] <- as.numeric(as.character(trainStore_i$SchoolHoliday[2:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-1)]))
  trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-2)] <- ifelse(trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-2)] > 0, 
                                                                           trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-2)], 
                                                                           as.numeric(as.character(trainStore_i$SchoolHoliday[3:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-2)])))
  trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-3)] <- ifelse(trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-3)] > 0, 
                                                                           trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-3)], 
                                                                           as.numeric(as.character(trainStore_i$SchoolHoliday[4:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-3)])))
  trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-4)] <- ifelse(trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-4)] > 0, 
                                                                           trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-4)], 
                                                                           as.numeric(as.character(trainStore_i$SchoolHoliday[5:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-4)])))
  trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-5)] <- ifelse(trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-5)] > 0, 
                                                                           trainStore_i$DaysWithinSchoolHoliday[1:(nrow(trainStore_i)-5)], 
                                                                           as.numeric(as.character(trainStore_i$SchoolHoliday[6:nrow(trainStore_i)])) & !as.numeric(as.character(trainStore_i$SchoolHoliday[1:(nrow(trainStore_i)-5)])))
  trainStore$DaysWithinSchoolHoliday[which(trainStore$Store == i)] <- trainStore_i$DaysWithinSchoolHoliday
}
trainStore$DaysWithinSchoolHoliday <- as.factor(trainStore$DaysWithinSchoolHoliday)

# Month
trainStore$Month <- month(trainStore$Date)
trainStore$Month <- as.factor(trainStore$Month)

# Remove all closed stores
trainStore$LogSales <- log(1+trainStore$Sales)
trainStoreOpen <- subset(trainStore, trainStore$Open == 1)

# Check all NAs in 
apply(trainStoreOpen,2,function(x) sum(is.na(x)))

# StoreSaleMean <- aggregate(Sales~Store, data = trainStoreOpen, FUN = mean)
# StoreSaleMedian <- aggregate(Sales~Store, data = trainStoreOpen, FUN = median)


####################################################
##### Take Store 1
####################################################
trainStoreOpen1 <- subset(trainStoreOpen, trainStoreOpen$Store == 1)
# Plot Sales as a function of Date
ggplot(data = trainStoreOpen1, aes(Date, Sales)) + geom_line()
plot2013 <- ggplot(data = subset(trainStoreOpen1, (trainStoreOpen1$Date >= as.Date("2013-01-01")) & (trainStoreOpen1$Date < as.Date("2014-01-01"))), aes(Date, Sales)) + geom_line()
plot2014 <- ggplot(data = subset(trainStoreOpen1, (trainStoreOpen1$Date >= as.Date("2014-01-01")) & (trainStoreOpen1$Date < as.Date("2015-01-01"))), aes(Date, Sales)) + geom_line()
grid.arrange(plot2013, plot2014, nrow = 2)
# There is an obvious time trend

# Time series analysis
Store1.diff <- diff(trainStoreOpen1$LogSales)
adf.test(trainStoreOpen1$LogSales, alternative="stationary", k=0)
adf.test(trainStoreOpen1$LogSales, alternative="stationary")
adf.test(trainStoreOpen1$LogSales, alternative="explosive", k=0)
adf.test(trainStoreOpen1$LogSales, alternative="explosive")

acf2(trainStoreOpen1$LogSales)
acf2(Store1.diff)

# Examine seasonal data
LogSales12 <- diff(trainStoreOpen1$LogSales, 12)
plot(LogSales12)
acf2(LogSales12)

###########################################################
### Add in Exonerous variables
###########################################################
apply(trainStoreOpen1,2,function(x) sum(is.na(x)))

xreg1 <- model.matrix(~ droplevels(as.factor(trainStoreOpen1$DayOfWeek)) 
                      + trainStoreOpen1$Promo
                      + trainStoreOpen1$SchoolHoliday
                      + trainStoreOpen1$ClosedDayPrior
                      + trainStoreOpen1$ClosingNextDay
                      + trainStoreOpen1$PromoWithin3Days
                      + trainStoreOpen1$PromoEndedWithin3Days
                      + trainStoreOpen1$TimePriorWasSchoolHoliday
                      + trainStoreOpen1$DaysWithinSchoolHoliday
                      + trainStoreOpen1$Month
                      )
xreg1 <- xreg1[,-1]
colnames(xreg1) <- c("Mon", "Sat", "Thur", "Tues", "Wed", 
                     "Promo", "SchoolHoliday", 
                     "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                     "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday",
                     "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                     )

Arima(trainStoreOpen1$LogSales, order = c(1,0,1), seasonal = list(order = c(0,0,1), period = 12))
# sigma^2 estimated as 0.02259:  log likelihood=373.49 AIC=-736.98   AICc=-736.9   BIC=-713.68
Arima(trainStoreOpen1$LogSales, order = c(1,0,1), seasonal = list(order = c(0,0,1), period = 12), xreg = xreg1)
# sigma^2 estimated as 0.01463:  log likelihood=546.71 AIC=-1069.43   AICc=-1069.02   BIC=-1013.5
# sigma^2 estimated as 0.01331:  log likelihood=592.56 AIC=-1127.11   AICc=-1124.8   BIC=-991.96

myARIMA.min <- Arima(trainStoreOpen1$LogSales, order = c(0,0,0), seasonal = list(order = c(0,0,0), period = 12), xreg = xreg1)
minAIC <- myARIMA.min$aic

for (i in 0:1) { for (j in 0:1) { for (k in 0:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# Arima(trainStoreOpen1$LogSales, order = c(1,0,4), seasonal = list(order = c(3,1,0), period = 12), xreg = xreg1)
# Arima(trainStoreOpen1$LogSales, order = c(1,0,4), seasonal = list(order = c(3,1,1), period = 12), xreg = xreg1)
# Arima(trainStoreOpen1$LogSales, order = c(1,0,4), seasonal = list(order = c(3,1,2), period = 12), xreg = xreg1)
# Arima(trainStoreOpen1$LogSales, order = c(1,0,4), seasonal = list(order = c(3,1,3), period = 12), xreg = xreg1)

for (i in 1:1) { for (j in 0:0) { for (k in 5:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# Arima(trainStoreOpen1$LogSales, order = c(1,0,5), seasonal = list(order = c(3,1,2), period = 12), xreg = xreg1)

for (i in 1:1) { for (j in 0:0) { for (k in 5:5) {
  for (l in 3:3) { for (m in 1:1) { for(n in 3:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# "Minimum AIC: -1167.3441648102"
# "Best ARIMA Model: (1,0,5),(3,0,3)"

for (i in 1:1) { for (j in 1:1) { for (k in 0:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# Arima(trainStoreOpen1$LogSales, order = c(1,1,1), seasonal = list(order = c(3,1,1), period = 12), xreg = xreg1)

for (i in 1:1) { for (j in 1:1) { for (k in 1:1) {
  for (l in 3:3) { for (m in 1:1) { for(n in 2:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

for (i in 1:1) { for (j in 1:1) { for (k in 2:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# Arima(trainStoreOpen1$LogSales, order = c(1,1,2), seasonal = list(order = c(1,1,3), period = 12), xreg = xreg1)

for (i in 1:1) { for (j in 1:1) { for (k in 2:2) {
  for (l in 2:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# Errors

for (i in 1:1) { for (j in 1:1) { for (k in 2:2) {
  for (l in 3:3) { for (m in 1:1) { for(n in 2:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

for (i in 1:1) { for (j in 1:1) { for (k in 3:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# Error

for (i in 1:1) { for (j in 1:1) { for (k in 5:5) {
  for (l in 1:1) { for (m in 1:1) { for(n in 3:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

for (i in 1:1) { for (j in 1:1) { for (k in 5:5) {
  for (l in 2:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# "Minimum AIC: -1167.3441648102"
# "Best ARIMA Model: (1,0,5),(3,0,3)"

for (i in 2:3) { for (j in 0:1) { for (k in 0:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

# Error

for (i in 2:2) { for (j in 0:0) { for (k in 1:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

for (i in 2:2) { for (j in 0:0) { for (k in 2:2) {
  for (l in 1:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

for (i in 2:2) { for (j in 0:0) { for (k in 3:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

for (i in 2:2) { for (j in 1:1) { for (k in 0:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }


for (i in 3:3) { for (j in 0:1) { for (k in 0:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

for (i in 4:6) { for (j in 0:1) { for (k in 0:5) {
  for (l in 0:3) { for (m in 0:1) { for(n in 0:3) {
    print(paste("Currently Working On: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
    myARIMA.temp <- Arima(trainStoreOpen1$LogSales, order = c(i,j,k), seasonal = list(order = c(l,m,n), period = 12), xreg = xreg1)
    if(minAIC > myARIMA.temp$aic) {
      print(paste("Minimum AIC: ", myARIMA.temp$aic, sep = ""))
      print(paste("Best ARIMA Model: ", "(", i, ",", j, ",", k, "),(", l, ",", m, ",", n, ")", sep = ""))
      minAIC <- myARIMA.temp$aic
      myARIMA.min <- myARIMA.temp
    }
  } } }
} } }

####################################################################################
### Split training data into different groups and train each subgroup separately
####################################################################################

# StoreSaleMean <- aggregate(Sales~Store, data = trainStoreOpen, FUN = mean)
# StoreSaleMedian <- aggregate(Sales~Store, data = trainStoreOpen, FUN = median)

# Work with median sale price, it's not as skewed

####################################################################################
### Creating New features from existing features