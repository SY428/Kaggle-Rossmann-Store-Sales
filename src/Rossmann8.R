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

trainStoreAvg$LogSales <- log(1+trainStoreAvg$Sales)
# Check all NAs in 
apply(trainStoreOpen,2,function(x) sum(is.na(x)))

###############################
## Do the same with test data
###############################
# Check all NAs in 
apply(testStore,2,function(x) sum(is.na(x)))

# Deal with NAs
testStore$Open[which(is.na(testStore$Open))] <- 1

testStore$Date <- as.Date(testStore$Date)
testStore$DayOfWeek <- as.character(testStore$DayOfWeek)
testStore$DayOfWeek[which(testStore$DayOfWeek == "1")] <- "Monday"
testStore$DayOfWeek[which(testStore$DayOfWeek == "2")] <- "Tuesday"
testStore$DayOfWeek[which(testStore$DayOfWeek == "3")] <- "Wednesday"
testStore$DayOfWeek[which(testStore$DayOfWeek == "4")] <- "Thursday"
testStore$DayOfWeek[which(testStore$DayOfWeek == "5")] <- "Friday"
testStore$DayOfWeek[which(testStore$DayOfWeek == "6")] <- "Saturday"
testStore$DayOfWeek[which(testStore$DayOfWeek == "7")] <- "Sunday"

testStore$DayOfWeek <- as.factor(testStore$DayOfWeek)
testStore$Open <- as.factor(testStore$Open)
testStore$Promo <- as.factor(testStore$Promo)
testStore$SchoolHoliday <- as.factor(testStore$SchoolHoliday)
testStore$Promo2 <- as.factor(testStore$Promo2)

# New Features

# Was Closed Yesterday
testStore$ClosedDayPrior <- 0
for (i in 1:max(unique(testStore$Store))) {
  if(i %in% unique(testStore$Store)) {
    testStore_i <- subset(testStore, testStore$Store == i)
    # Assume first one open, switch for test case
    testStore_i$ClosedDayPrior[2:nrow(testStore_i)] <- as.numeric(!as.numeric(as.character(testStore_i$Open[1:(nrow(testStore_i)-1)])))
    testStore$ClosedDayPrior[which(testStore$Store == i)] <- testStore_i$ClosedDayPrior
  }
}
testStore$ClosedDayPrior <- as.factor(testStore$ClosedDayPrior)

# Will Close Tomorrow
testStore$ClosingNextDay <- 0
for (i in 1:max(unique(testStore$Store))) {
  if(i %in% unique(testStore$Store)) {
    testStore_i <- subset(testStore, testStore$Store == i)
    # Assume last one open, switch for test case
    testStore_i$ClosingNextDay[1:(nrow(testStore_i)-1)] <- as.numeric(!as.numeric(as.character(testStore_i$Open[2:nrow(testStore_i)])))
    testStore$ClosingNextDay[which(testStore$Store == i)] <- testStore_i$ClosingNextDay
  }
}
testStore$ClosingNextDay <- as.factor(testStore$ClosingNextDay)

# Will Have Promo in less than 3 days and not promo day
testStore$PromoWithin3Days <- 0
for (i in 1:max(unique(testStore$Store))) {
  if(i %in% unique(testStore$Store)) {
    testStore_i <- subset(testStore, testStore$Store == i)
    # Assume last one open, switch for test case
    testStore_i$PromoWithin3Days[1:(nrow(testStore_i)-1)] <- as.numeric(as.character(testStore_i$Promo[2:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$Promo[1:(nrow(testStore_i)-1)]))
    testStore_i$PromoWithin3Days[1:(nrow(testStore_i)-2)] <- ifelse(testStore_i$PromoWithin3Days[1:(nrow(testStore_i)-2)] > 0, 
                                                                    testStore_i$PromoWithin3Days[1:(nrow(testStore_i)-2)], 
                                                                    as.numeric(as.character(testStore_i$Promo[3:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$Promo[1:(nrow(testStore_i)-2)])))
    testStore_i$PromoWithin3Days[1:(nrow(testStore_i)-3)] <- ifelse(testStore_i$PromoWithin3Days[1:(nrow(testStore_i)-3)] > 0, 
                                                                    testStore_i$PromoWithin3Days[1:(nrow(testStore_i)-3)], 
                                                                    as.numeric(as.character(testStore_i$Promo[4:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$Promo[1:(nrow(testStore_i)-3)])))
    testStore$PromoWithin3Days[which(testStore$Store == i)] <- testStore_i$PromoWithin3Days
  }
}
testStore$PromoWithin3Days <- as.factor(testStore$PromoWithin3Days)

# Promo Ended less than 3 days ago
testStore$PromoEndedWithin3Days <- 0
for (i in 1:max(unique(testStore$Store))) {
  if(i %in% unique(testStore$Store)) {
    testStore_i <- subset(testStore, testStore$Store == i)
    # Assume first one open, switch for test case
    testStore_i$PromoEndedWithin3Days[2:nrow(testStore_i)] <- as.numeric(as.character(testStore_i$Promo[1:(nrow(testStore_i)-1)])) & !as.numeric(as.character(testStore_i$Promo[2:nrow(testStore_i)]))
    testStore_i$PromoEndedWithin3Days[3:nrow(testStore_i)] <- ifelse(testStore_i$PromoEndedWithin3Days[3:nrow(testStore_i)] > 0, 
                                                                     testStore_i$PromoEndedWithin3Days[3:nrow(testStore_i)], 
                                                                     as.numeric(as.character(testStore_i$Promo[1:(nrow(testStore_i)-2)])) & !as.numeric(as.character(testStore_i$Promo[3:nrow(testStore_i)])))
    testStore_i$PromoEndedWithin3Days[4:nrow(testStore_i)] <- ifelse(testStore_i$PromoEndedWithin3Days[4:nrow(testStore_i)] > 0, 
                                                                     testStore_i$PromoEndedWithin3Days[4:nrow(testStore_i)], 
                                                                     as.numeric(as.character(testStore_i$Promo[1:(nrow(testStore_i)-3)])) & !as.numeric(as.character(testStore_i$Promo[4:nrow(testStore_i)])))
    
    
    testStore$PromoEndedWithin3Days[which(testStore$Store == i)] <- testStore_i$PromoEndedWithin3Days
  }
}
testStore$PromoEndedWithin3Days <- as.factor(testStore$PromoEndedWithin3Days)

# Was school holiday within 5 days
testStore$TimePriorWasSchoolHoliday <- 0
for (i in 1:max(unique(testStore$Store))) {
  if(i %in% unique(testStore$Store)) {
    testStore_i <- subset(testStore, testStore$Store == i)
    # Assume first one open, switch for test case
    testStore_i$TimePriorWasSchoolHoliday[2:nrow(testStore_i)] <- as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-1)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[2:nrow(testStore_i)]))
    testStore_i$TimePriorWasSchoolHoliday[3:nrow(testStore_i)] <- ifelse(testStore_i$TimePriorWasSchoolHoliday[3:nrow(testStore_i)] > 0, 
                                                                         testStore_i$TimePriorWasSchoolHoliday[3:nrow(testStore_i)], 
                                                                         as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-2)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[3:nrow(testStore_i)])))
    testStore_i$TimePriorWasSchoolHoliday[4:nrow(testStore_i)] <- ifelse(testStore_i$TimePriorWasSchoolHoliday[4:nrow(testStore_i)] > 0, 
                                                                         testStore_i$TimePriorWasSchoolHoliday[4:nrow(testStore_i)], 
                                                                         as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-3)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[4:nrow(testStore_i)])))
    testStore_i$TimePriorWasSchoolHoliday[5:nrow(testStore_i)] <- ifelse(testStore_i$TimePriorWasSchoolHoliday[5:nrow(testStore_i)] > 0, 
                                                                         testStore_i$TimePriorWasSchoolHoliday[5:nrow(testStore_i)], 
                                                                         as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-4)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[5:nrow(testStore_i)])))
    testStore_i$TimePriorWasSchoolHoliday[6:nrow(testStore_i)] <- ifelse(testStore_i$TimePriorWasSchoolHoliday[6:nrow(testStore_i)] > 0, 
                                                                         testStore_i$TimePriorWasSchoolHoliday[6:nrow(testStore_i)], 
                                                                         as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-5)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[6:nrow(testStore_i)])))
    testStore$TimePriorWasSchoolHoliday[which(testStore$Store == i)] <- testStore_i$TimePriorWasSchoolHoliday
  }
}
testStore$TimePriorWasSchoolHoliday <- as.factor(testStore$TimePriorWasSchoolHoliday)

# Going to be school holiday within 5 days
testStore$DaysWithinSchoolHoliday <- 0
for (i in 1:max(unique(testStore$Store))) {
  if(i %in% unique(testStore$Store)) {
    testStore_i <- subset(testStore, testStore$Store == i)
    # Assume last one open, switch for test case
    testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-1)] <- as.numeric(as.character(testStore_i$SchoolHoliday[2:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-1)]))
    testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-2)] <- ifelse(testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-2)] > 0, 
                                                                           testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-2)], 
                                                                           as.numeric(as.character(testStore_i$SchoolHoliday[3:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-2)])))
    testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-3)] <- ifelse(testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-3)] > 0, 
                                                                           testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-3)], 
                                                                           as.numeric(as.character(testStore_i$SchoolHoliday[4:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-3)])))
    testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-4)] <- ifelse(testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-4)] > 0, 
                                                                           testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-4)], 
                                                                           as.numeric(as.character(testStore_i$SchoolHoliday[5:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-4)])))
    testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-5)] <- ifelse(testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-5)] > 0, 
                                                                           testStore_i$DaysWithinSchoolHoliday[1:(nrow(testStore_i)-5)], 
                                                                           as.numeric(as.character(testStore_i$SchoolHoliday[6:nrow(testStore_i)])) & !as.numeric(as.character(testStore_i$SchoolHoliday[1:(nrow(testStore_i)-5)])))
    testStore$DaysWithinSchoolHoliday[which(testStore$Store == i)] <- testStore_i$DaysWithinSchoolHoliday
  }
}
testStore$DaysWithinSchoolHoliday <- as.factor(testStore$DaysWithinSchoolHoliday)

# Month
testStore$Month <- month(testStore$Date)
testStore$Month <- as.factor(testStore$Month)

# Remove all closed stores
testStore$LogSales <- 0
testStore$Sales <- 0
testStoreOpen <- subset(testStore, testStore$Open == 1)

# Check all NAs in 
apply(testStore,2,function(x) sum(is.na(x)))

####################################################
##### Take Store 1
####################################################
trainStore1 <- subset(trainStoreAvg, trainStoreAvg$Store == 1)

apply(trainStore1,2,function(x) sum(is.na(x)))

xreg1 <- model.matrix(~ droplevels(as.factor(trainStore1$DayOfWeek)) 
                      + trainStore1$Promo
                      + trainStore1$SchoolHoliday
                      + trainStore1$ClosedDayPrior
                      + trainStore1$ClosingNextDay
                      + trainStore1$PromoWithin3Days
                      + trainStore1$PromoEndedWithin3Days
                      + trainStore1$TimePriorWasSchoolHoliday
                      + trainStore1$DaysWithinSchoolHoliday
                      + trainStore1$Open
)
xreg1 <- xreg1[,-1]
colnames(xreg1) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                     "Promo", "SchoolHoliday", 
                     "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                     "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday",
                     "Open"
)

# "Minimum AIC: -1167.3441648102"
# "Best ARIMA Model: (1,0,5),(3,0,3)"

# myARIMA <- Arima(trainStoreOpen1$LogSales, order = c(1,0,5), seasonal = list(order = c(3,0,3), period = 12), xreg = xreg1)

myAutoArima <- auto.arima(ts(trainStore1$LogSales), xreg = xreg1, lambda = BoxCox.lambda(ts(trainStore1$LogSales)))

# auto.arima(ts(trainStore1$LogSales), xreg = xreg1, lambda = BoxCox.lambda(ts(trainStore1$LogSales)))
# auto.arima(ts(trainStore1$LogSales), xreg = xreg1)
# auto.arima(ts(trainStore1$LogSales), xreg = xreg1, D = 1)
# auto.arima(ts(trainStore1$LogSales), xreg = xreg1, D = 1, lambda = BoxCox.lambda(ts(trainStore1$LogSales)))

# Predict test store 1
testStore1 <- subset(testStore, testStore$Store == 1)
# testStoreOpen1 <- subset(testStoreOpen, testStoreOpen$Store == 1)

newxreg1 <- model.matrix(~ droplevels(as.factor(testStore1$DayOfWeek)) 
                         + testStore1$Promo
                         + testStore1$SchoolHoliday
                         + testStore1$ClosedDayPrior
                         + testStore1$ClosingNextDay
                         + testStore1$PromoWithin3Days
                         + testStore1$PromoEndedWithin3Days
                         + testStore1$TimePriorWasSchoolHoliday
                         + testStore1$DaysWithinSchoolHoliday
                         + testStore1$Open
)
newxreg1 <- newxreg1[,-1]
colnames(newxreg1) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                        "Promo", "SchoolHoliday", 
                        "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                        "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday",
                        "Open"
)

pred.date <- as.Date(unique(testStore1$Date))
predOpen.date <- as.Date(unique(testStore1$Date))
# ggplot(data = subset(trainStoreOpen1, trainStoreOpen1$Date > as.Date("2015-07-01")), aes(Date, LogSales)) + geom_line()

predLogSales <- data.frame(forecast(myAutoArima, xreg = newxreg1, lambda = myAutoArima$lambda))
predSales <- exp(predLogSales$Point.Forecast)-1

# trainSamplePred <- predict(myARIMA, newxreg = newxreg1, n.ahead = length(predOpen.date))
plot2013_range <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2013-08-01")) & (trainStore1$Date < as.Date("2013-09-18"))), aes(Date, LogSales)) + geom_line()
plot2014_range <- ggplot(data = subset(trainStore1, (trainStore1$Date >= as.Date("2014-08-01")) & (trainStore1$Date < as.Date("2014-09-18"))), aes(Date, LogSales)) + geom_line()
plot2015_range <- ggplot(data = data.frame(Date = as.Date(pred.date), LogSales = predLogSales$Point.Forecast), aes(x = Date, y = LogSales)) + geom_line()
grid.arrange(plot2013_range, plot2014_range, plot2015_range, nrow = 3)

testStore1$LogSales <- predLogSales$Point.Forecast
testStore1$Sales <- exp(predLogSales$Point.Forecast)-1

# Assign all closed days to 0
testStore1$Sales[which(testStore1$Open == 0)] <- 0

####################################################
##### Take Store 3
####################################################
trainStore3 <- subset(trainStoreAvg, trainStoreAvg$Store == 3)

apply(trainStore3,2,function(x) sum(is.na(x)))

xreg3 <- model.matrix(~ droplevels(as.factor(trainStore3$DayOfWeek)) 
                      + trainStore3$Promo
                      + trainStore3$SchoolHoliday
                      + trainStore3$ClosedDayPrior
                      + trainStore3$ClosingNextDay
                      + trainStore3$PromoWithin3Days
                      + trainStore3$PromoEndedWithin3Days
                      + trainStore3$TimePriorWasSchoolHoliday
                      + trainStore3$DaysWithinSchoolHoliday
                      + trainStore3$Open
)
xreg3 <- xreg3[,-1]
colnames(xreg3) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                     "Promo", "SchoolHoliday", 
                     "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                     "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday",
                     "Open"
)

myAutoArima <- auto.arima(ts(trainStore3$LogSales), xreg = xreg3, lambda = BoxCox.lambda(ts(trainStore3$LogSales)))
testStore3 <- subset(testStore, testStore$Store == 3)

newxreg3 <- model.matrix(~ droplevels(as.factor(testStore3$DayOfWeek)) 
                         + testStore3$Promo
                         + testStore3$SchoolHoliday
                         + testStore3$ClosedDayPrior
                         + testStore3$ClosingNextDay
                         + testStore3$PromoWithin3Days
                         + testStore3$PromoEndedWithin3Days
                         + testStore3$TimePriorWasSchoolHoliday
                         + testStore3$DaysWithinSchoolHoliday
                         + testStore3$Open
)
newxreg3 <- newxreg3[,-1]
colnames(newxreg3) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                        "Promo", "SchoolHoliday", 
                        "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                        "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday",
                        "Open"
)

pred.date <- as.Date(unique(testStore3$Date))
predOpen.date <- as.Date(unique(testStore3$Date))

predLogSales <- data.frame(forecast(myAutoArima, xreg = newxreg3, lambda = myAutoArima$lambda))
predSales <- exp(predLogSales$Point.Forecast)-1

plot2013_range <- ggplot(data = subset(trainStore3, (trainStore3$Date >= as.Date("2013-08-01")) & (trainStore3$Date < as.Date("2013-09-18"))), aes(Date, LogSales)) + geom_line()
plot2014_range <- ggplot(data = subset(trainStore3, (trainStore3$Date >= as.Date("2014-08-01")) & (trainStore3$Date < as.Date("2014-09-18"))), aes(Date, LogSales)) + geom_line()
plot2015_range <- ggplot(data = data.frame(Date = as.Date(pred.date), LogSales = predLogSales$Point.Forecast), aes(x = Date, y = LogSales)) + geom_line()
grid.arrange(plot2013_range, plot2014_range, plot2015_range, nrow = 3)

testStore3$LogSales <- predLogSales$Point.Forecast
testStore3$Sales <- exp(predLogSales$Point.Forecast)-1

####################################################
##### Take Store 262
####################################################
trainStore262 <- subset(trainStoreAvg, trainStoreAvg$Store == 262)

acf2(ts(trainStore262$Sales))
plot(ts(trainStore262$Sales))
ggplot(data = trainStore262, aes(Date, LogSales)) + geom_line()

apply(trainStore262,2,function(x) sum(is.na(x)))

xreg262 <- model.matrix(~ droplevels(as.factor(trainStore262$DayOfWeek)) 
                      + trainStore262$Promo
                      + trainStore262$SchoolHoliday
                      + trainStore262$ClosedDayPrior
                      + trainStore262$ClosingNextDay
                      + trainStore262$PromoWithin3Days
                      + trainStore262$PromoEndedWithin3Days
                      + trainStore262$TimePriorWasSchoolHoliday
                      + trainStore262$DaysWithinSchoolHoliday
                      + trainStore262$Open
)
xreg262 <- xreg262[,-1]
colnames(xreg262) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                     "Promo", "SchoolHoliday", 
                     "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                     "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday",
                     "Open"
)

myAutoArima <- auto.arima(ts(trainStore262$LogSales), xreg = xreg262, lambda = BoxCox.lambda(ts(trainStore262$LogSales)))
testStore262 <- subset(testStore, testStore$Store == 262)

newxreg262 <- model.matrix(~ droplevels(as.factor(testStore262$DayOfWeek)) 
                         + testStore262$Promo
                         + testStore262$SchoolHoliday
                         + testStore262$ClosedDayPrior
                         + testStore262$ClosingNextDay
                         + testStore262$PromoWithin3Days
                         + testStore262$PromoEndedWithin3Days
                         + testStore262$TimePriorWasSchoolHoliday
                         + testStore262$DaysWithinSchoolHoliday
                         + testStore262$Open
)
newxreg262 <- newxreg262[,-1]
colnames(newxreg262) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                        "Promo", "SchoolHoliday", 
                        "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                        "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday",
                        "Open"
)

pred.date <- as.Date(unique(testStore262$Date))
predOpen.date <- as.Date(unique(testStore262$Date))

predLogSales <- data.frame(forecast(myAutoArima, xreg = newxreg262, lambda = myAutoArima$lambda))
predSales <- exp(predLogSales$Point.Forecast)-1

plot2013_range <- ggplot(data = subset(trainStore262, (trainStore262$Date >= as.Date("2013-08-01")) & (trainStore262$Date < as.Date("2013-09-18"))), aes(Date, LogSales)) + geom_line()
plot2014_range <- ggplot(data = subset(trainStore262, (trainStore262$Date >= as.Date("2014-08-01")) & (trainStore262$Date < as.Date("2014-09-18"))), aes(Date, LogSales)) + geom_line()
plot2015_range <- ggplot(data = data.frame(Date = as.Date(pred.date), LogSales = predLogSales$Point.Forecast), aes(x = Date, y = LogSales)) + geom_line()
grid.arrange(plot2013_range, plot2014_range, plot2015_range, nrow = 3)

testStore262$LogSales <- predLogSales$Point.Forecast
testStore262$Sales <- exp(predLogSales$Point.Forecast)-1

####################################################################################
### Find all stores in test that have unique features
####################################################################################
predictor_lst <- names(trainStoreAvg)
remove_names <- c("Store", "Date", "Sales", "Customers", "LogSales", "week_mean_sales", "year_week")
predictor_lst <- predictor_lst[!(predictor_lst %in% remove_names)]

simple_lst <- c("DayOfWeek", "Promo", "SchoolHoliday", 
                "ClosedDayPrior", "ClosingNextDay", 
                "PromoWithin3Days", "PromoEndedWithin3Days", 
                "TimePriorWasSchoolHoliday", 
                "DaysWithinSchoolHoliday", "Open")

list_of_stores <- list()

for (i in 1:length(simple_lst)) {
  print(simple_lst[i])
  stores_list <- as.numeric()
  for (j in 1:max(testStore$Store)) {
    if (j %in% unique(testStore$Store)) {
      if (length(unique(subset(trainStoreAvg, trainStoreAvg$Store == j)[[simple_lst[i]]])) < 2) {
        stores_list <- c(stores_list, j)
      }
    }
  }
  list_of_stores[length(list_of_stores)+1] <- list(list(simple_lst[i], stores_list))
  print(stores_list)
}

SpecialStores <- c(262, 335, 562, 733, 769, 1097)

####################################################################################
### For looping over all stores
####################################################################################

testPredSales <- as.numeric()
testId <- as.numeric()

NegStores <- as.numeric()
InfStores <- as.numeric()

####################################
### Regular Cases:
####################################

for (i in 1:max(testStore$Store)) {
  if ((i %in% unique(testStore$Store)) & !(i %in% SpecialStores)) {
    print(paste("Store: ", i, sep = ""))
    trainStore_i <- subset(trainStoreAvg, trainStoreAvg$Store == i)
    xreg_i <- model.matrix(~ droplevels(as.factor(trainStore_i$DayOfWeek)) 
                           + trainStore_i$Promo
                           + trainStore_i$SchoolHoliday
                           + trainStore_i$ClosedDayPrior
                           + trainStore_i$ClosingNextDay
                           + trainStore_i$PromoWithin3Days
                           + trainStore_i$PromoEndedWithin3Days
                           + trainStore_i$TimePriorWasSchoolHoliday
                           + trainStore_i$DaysWithinSchoolHoliday
                           + trainStore_i$Open
    )
    xreg_i <- xreg_i[,-1]
    colnames(xreg_i) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                          "Promo", 
                          "SchoolHoliday", 
                          "ClosedDayPrior", 
                          "ClosingNextDay", 
                          "PromoWithin3Days", 
                          "PromoEndedWithin3Days", 
                          "TimePriorWasSchoolHoliday", 
                          "DaysWithinSchoolHoliday",
                          "Open"
    )
    
    myAutoArima <- auto.arima(ts(trainStore_i$LogSales), xreg = xreg_i, lambda = BoxCox.lambda(ts(trainStore_i$LogSales)))
    print(myAutoArima)
    
    testStore_i <- subset(testStore, testStore$Store == i)
    newxreg_i <- model.matrix(~ droplevels(as.factor(testStore_i$DayOfWeek)) 
                              + testStore_i$Promo
                              + testStore_i$SchoolHoliday
                              + testStore_i$ClosedDayPrior
                              + testStore_i$ClosingNextDay
                              + testStore_i$PromoWithin3Days
                              + testStore_i$PromoEndedWithin3Days
                              + testStore_i$TimePriorWasSchoolHoliday
                              + testStore_i$DaysWithinSchoolHoliday
                              + testStore_i$Open
    )
    newxreg_i <- newxreg_i[,-1]
    colnames(newxreg_i) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                             "Promo", 
                             "SchoolHoliday", 
                             "ClosedDayPrior", 
                             "ClosingNextDay", 
                             "PromoWithin3Days", 
                             "PromoEndedWithin3Days", 
                             "TimePriorWasSchoolHoliday", 
                             "DaysWithinSchoolHoliday",
                             "Open"
    )
    
    predLogSales_i <- data.frame(forecast(myAutoArima, xreg = newxreg_i, lambda = myAutoArima$lambda))
    testStore_i$LogSales <- predLogSales_i$Point.Forecast
    testStore_i$Sales <- exp(predLogSales_i$Point.Forecast)-1
    
    # Assign all closed days to 0
    testStore_i$Sales[which(testStore_i$Open == 0)] <- 0
    
    # Manage Negative and Inf values
    if (length(which(testStore_i$Sales == Inf)) > 0) {
      InfStores <- c(InfStores, i)
    }
    
    if (length(which(testStore_i$Sales < 0)) > 0) {
      NegStores <- c(NegStores, i)
    }
    
    testStore_i$Sales[testStore_i$Sales < 0] <- 0
    testStore_i$Sales[testStore_i$Sales == Inf] <- max(testStore_i$Sales[testStore_i$Sales != Inf])
    
    testPredSales <- c(testPredSales, testStore_i$Sales)
    testId <- c(testId, testStore_i$Id)
  }
}

####################################
### Special Cases: SpecialStores
####################################

for (i in min(SpecialStores):max(SpecialStores)) {
  if (i %in% SpecialStores) {
    print(paste("Store: ", i, sep = ""))
    trainStore_i <- subset(trainStoreAvg, trainStoreAvg$Store == i)
    xreg_i <- model.matrix(~ droplevels(as.factor(trainStore_i$DayOfWeek)) 
                           + trainStore_i$Promo
                           + trainStore_i$SchoolHoliday
                           # + trainStore_i$ClosedDayPrior
                           # + trainStore_i$ClosingNextDay
                           + trainStore_i$PromoWithin3Days
                           + trainStore_i$PromoEndedWithin3Days
                           + trainStore_i$TimePriorWasSchoolHoliday
                           + trainStore_i$DaysWithinSchoolHoliday
                           # + trainStore_i$Open
    )
    xreg_i <- xreg_i[,-1]
    colnames(xreg_i) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                          "Promo", 
                          "SchoolHoliday", 
                          # "ClosedDayPrior", 
                          # "ClosingNextDay", 
                          "PromoWithin3Days", 
                          "PromoEndedWithin3Days", 
                          "TimePriorWasSchoolHoliday", 
                          "DaysWithinSchoolHoliday"
                          # "Open"
    )
    
    myAutoArima <- auto.arima(ts(trainStore_i$LogSales), xreg = xreg_i, lambda = BoxCox.lambda(ts(trainStore_i$LogSales)))
    print(myAutoArima)
    
    testStore_i <- subset(testStore, testStore$Store == i)
    newxreg_i <- model.matrix(~ droplevels(as.factor(testStore_i$DayOfWeek)) 
                              + testStore_i$Promo
                              + testStore_i$SchoolHoliday
                              # + testStore_i$ClosedDayPrior
                              # + testStore_i$ClosingNextDay
                              + testStore_i$PromoWithin3Days
                              + testStore_i$PromoEndedWithin3Days
                              + testStore_i$TimePriorWasSchoolHoliday
                              + testStore_i$DaysWithinSchoolHoliday
                              # + testStore_i$Open
    )
    newxreg_i <- newxreg_i[,-1]
    colnames(newxreg_i) <- c("Mon", "Sat", "Sun", "Thur", "Tues", "Wed", 
                             "Promo", 
                             "SchoolHoliday", 
                             # "ClosedDayPrior", 
                             # "ClosingNextDay", 
                             "PromoWithin3Days", 
                             "PromoEndedWithin3Days", 
                             "TimePriorWasSchoolHoliday", 
                             "DaysWithinSchoolHoliday"
                             # "Open"
    )
    
    predLogSales_i <- data.frame(forecast(myAutoArima, xreg = newxreg_i, lambda = myAutoArima$lambda))
    testStore_i$LogSales <- predLogSales_i$Point.Forecast
    testStore_i$Sales <- exp(predLogSales_i$Point.Forecast)-1
    
    # Assign all closed days to 0
    testStore_i$Sales[which(testStore_i$Open == 0)] <- 0
    
    # Manage Negative and Inf values
    if (length(which(testStore_i$Sales == Inf)) > 0) {
      InfStores <- c(InfStores, i)
    }
    
    if (length(which(testStore_i$Sales < 0)) > 0) {
      NegStores <- c(NegStores, i)
    }
    
    testStore_i$Sales[testStore_i$Sales < 0] <- 0
    testStore_i$Sales[testStore_i$Sales == Inf] <- max(testStore_i$Sales[testStore_i$Sales != Inf])
    
    testPredSales <- c(testPredSales, testStore_i$Sales)
    testId <- c(testId, testStore_i$Id)
  }
}

summary(testId)
summary(testPredSales)

testPred <- data.frame(Id = testId, Sales = testPredSales)

Id <- testId
Sales <- testPredSales
output <- data.frame(Id, Sales)
write.csv(output, file = "RossmannSimple2.csv", row.names = FALSE)
