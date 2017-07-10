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
StoreSaleMean <- aggregate(Sales~Store, data = trainStore, FUN = mean)
StoreSaleMedian <- aggregate(Sales~Store, data = trainStore, FUN = median)
StoreOpenPercent <- aggregate(as.numeric(as.character(Open))~Store, data = trainStore, FUN = mean)
names(StoreOpenPercent) <- c("Store", "OpenPercent")


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

# Replace all closed days sales with avg values of open days of the week
trainStoreAvg <- trainStore
trainStoreAvg$week_mean_sales <- 0
year <- year(trainStoreAvg$Date)
week <- week(trainStoreAvg$Date)
year_week <- paste(year, week, sep = "_")
trainStoreAvg <- data.frame(trainStoreAvg, year_week = year_week)

for (i in 1:999) {
  print(i)
  trainStore_i <- subset(trainStoreAvg, trainStoreAvg$Store == i)
  trainStoreOpen_i <- subset(trainStore_i, trainStore_i$Open == 1)
  week_mean_sales <- aggregate(Sales~year_week, data = trainStoreOpen_i, FUN = mean)
  names(week_mean_sales) <- c("year_week", "week_mean_sales")
  if (length(unique(year_week)[which(!(unique(year_week) %in% (unique(week_mean_sales$year_week))))]) > 0) {
    week_mean_sales <- rbind(week_mean_sales, data.frame(year_week = unique(year_week)[which(!(unique(year_week) %in% (unique(week_mean_sales$year_week))))], week_mean_sales = rep(0, length(unique(year_week)[which(!(unique(year_week) %in% (unique(week_mean_sales$year_week))))]))))
  }
  trainStoreAvg[which(trainStoreAvg$Store == i),]$week_mean_sales <- week_mean_sales[match(trainStoreAvg[which(trainStoreAvg$Store == i),]$year_week, week_mean_sales$year_week),2]
  if(sum(is.na(trainStoreAvg$week_mean_sales[which(trainStoreAvg$Open == 0)]))>0) {
    print(paste(i," has NAs", sep = ""))
  }
}

for (i in 1000:length(unique(trainStoreAvg$Store))) {
  print(i)
  trainStore_i <- subset(trainStoreAvg, trainStoreAvg$Store == i)
  trainStoreOpen_i <- subset(trainStore_i, trainStore_i$Open == 1)
  week_mean_sales <- aggregate(Sales~year_week, data = trainStoreOpen_i, FUN = mean)
  names(week_mean_sales) <- c("year_week", "week_mean_sales")
  if (length(unique(year_week)[which(!(unique(year_week) %in% (unique(week_mean_sales$year_week))))]) > 0) {
    week_mean_sales <- rbind(week_mean_sales, data.frame(year_week = unique(year_week)[which(!(unique(year_week) %in% (unique(week_mean_sales$year_week))))], week_mean_sales = rep(0, length(unique(year_week)[which(!(unique(year_week) %in% (unique(week_mean_sales$year_week))))]))))
  }
  trainStoreAvg[which(trainStoreAvg$Store == i),]$week_mean_sales <- week_mean_sales[match(trainStoreAvg[which(trainStoreAvg$Store == i),]$year_week, week_mean_sales$year_week),2]
  if(sum(is.na(trainStoreAvg$week_mean_sales[which(trainStoreAvg$Open == 0)]))>0) {
    print(paste(i," has NAs", sep = ""))
  }
}

trainStoreAvg$Sales[which(trainStoreAvg$Open == 0)] <- trainStoreAvg$week_mean_sales[which(trainStoreAvg$Open == 0)] 

# Check all NAs in 
apply(trainStoreAvg,2,function(x) sum(is.na(x)))

####################################################
##### Take Store 1
####################################################
trainStore1 <- subset(trainStoreAvg, trainStoreAvg$Store == 1)
trainStore1Open <- subset(trainStore1, trainStore1$Open == 1)

# Plot Sales as a function of Date
ggplot(data = trainStore1, aes(Date, Sales)) + geom_line()
plot2013 <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2013-01-01")) & (trainStore1$Date < as.Date("2014-01-01"))), aes(Date, Sales)) + geom_line()
plot2014 <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2014-01-01")) & (trainStore1$Date < as.Date("2015-01-01"))), aes(Date, Sales)) + geom_line()
grid.arrange(plot2013, plot2014, nrow = 2)
# There is an obvious time trend

# Time series analysis
adf.test(trainStore1$Sales, alternative="stationary", k=0)
adf.test(trainStore1$Sales, alternative="stationary")
adf.test(trainStore1$Sales, alternative="explosive", k=0)
adf.test(trainStore1$Sales, alternative="explosive")

acf2(trainStore1$Sales)

Store1.diff <- diff(trainStore1$Sales)
acf2(Store1.diff)

# Examine seasonal data
Sales12 <- diff(trainStore1$Sales, 12)
plot(Sales12)
acf2(Sales12)

###########################################################
### Add in Exonerous variables
###########################################################
xreg1 <- model.matrix(~as.factor(trainStore1$DayOfWeek))
xreg1 <- xreg1[,-1]
colnames(xreg1) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed")

Arima(trainStore1$Sales, order = c(0,0,0), seasonal = list(order = c(0,0,1), period = 12))
Arima(trainStore1Open$Sales, order = c(1,0,1), xreg = xreg1, seasonal = list(order = c(0,0,1), period = 12))

####################################################################################
### Split training data into different groups and train each subgroup separately
####################################################################################

# Variables that are store dependent vs variables that are not

# Store dependent variables

# Non Store depedent variables

