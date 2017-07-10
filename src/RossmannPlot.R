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
##### Plot Store Training Data
####################################################

k <- 125

if (1) {
  k <- k + 1
  Store_i <- unique(testStore$Store)[k]
  for (i in Store_i:Store_i) {
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
      
      pred.date <- as.Date(unique(testStore_i$Date))
      
      # Assign all closed days to 0
      # testStore_i$Sales[which(testStore_i$Open == 0)] <- 0
      
      # testPredSales <- c(testPredSales, testStore_i$Sales)
      # testId <- c(testId, testStore_i$Id)
      
      plot_train <- ggplot(data = trainStore_i, aes(Date, LogSales)) + geom_line() + ggtitle(paste("Plot for Store: ", i, sep = ""))
      plot2013_range <- ggplot(data = subset(trainStore_i, (trainStore_i$Date >= as.Date("2013-08-01")) & (trainStore_i$Date < as.Date("2013-09-18"))), aes(Date, LogSales)) + geom_line()
      plot2014_range <- ggplot(data = subset(trainStore_i, (trainStore_i$Date >= as.Date("2014-08-01")) & (trainStore_i$Date < as.Date("2014-09-18"))), aes(Date, LogSales)) + geom_line()
      plot2015_range <- ggplot(data = data.frame(Date = as.Date(pred.date), LogSales = predLogSales_i$Point.Forecast), aes(x = Date, y = LogSales)) + geom_line()
      grid.arrange(plot_train, plot2013_range, plot2014_range, plot2015_range, nrow = 4)
  }
}

####################################################
##### Stores Needing Special Care
####################################################

GapStores <- c(13, 20, 22, 25, 30, 32, 36, 41, 46, 51, 52, 58, 72, 76, 81, 89, 99, 100,
               102, 105, 108, 113, 115, 120, 127, 129, 132, 136, 137, 139, 144, 145, 149,
               155, 159, 164)

##########################################################
##### Stores with days of consecutive closing
##########################################################

ThreeOrMoreDays <- as.numeric()

for (i in 1:max(testStore$Store)) {
  if (i %in% unique(testStore$Store)) {
    trainStore_i <- subset(trainStore, trainStore$Store == i)
    OpenVec_i <- trainStore_i$Open
    OpenString <- gsub(", ", "", toString(as.character(OpenVec_i)))
    if(grepl("000", OpenString)) {
      ThreeOrMoreDays <- c(ThreeOrMoreDays, i)
    }
  }
}

SixOrMoreDays <- as.numeric()

for (i in 1:max(testStore$Store)) {
  if (i %in% unique(testStore$Store)) {
    trainStore_i <- subset(trainStore, trainStore$Store == i)
    OpenVec_i <- trainStore_i$Open
    OpenString <- gsub(", ", "", toString(as.character(OpenVec_i)))
    if(grepl("000000", OpenString)) {
      SixOrMoreDays <- c(SixOrMoreDays, i)
    }
  }
}

TenOrMoreDays <- as.numeric()

for (i in 1:max(testStore$Store)) {
  if (i %in% unique(testStore$Store)) {
    trainStore_i <- subset(trainStore, trainStore$Store == i)
    OpenVec_i <- trainStore_i$Open
    OpenString <- gsub(", ", "", toString(as.character(OpenVec_i)))
    if(grepl("0000000000", OpenString)) {
      TenOrMoreDays <- c(TenOrMoreDays, i)
    }
  }
}

##########################################################
##### Stores With 3-5 days of consecutive closing
##########################################################

ThreeToFiveDaysClosed <- ThreeOrMoreDays[which(!(ThreeOrMoreDays %in% SixOrMoreDays))]
  
##########################################################
##### Stores With 6-9 days of consecutive closing
##########################################################

SixToNineDaysClosed <- SixOrMoreDays[which(!(SixOrMoreDays %in% TenOrMoreDays))]

##########################################################
##### Stores With >= 10 days of consecutive closing
##########################################################

TenOrMoreDays

##########################################################
##### Stores With Gap or Missing Days
##########################################################

StoresWithMissingDays <- as.numeric()
MissingNumDays <- as.numeric()

for (i in 1:max(testStore$Store)) {
  if (i %in% unique(testStore$Store)) {
    trainStore_i <- subset(trainStore, trainStore$Store == i)
    if ((max(trainStore_i$Date) - min(trainStore_i$Date)) > length(trainStore_i$Date)) {
      StoresWithMissingDays <- c(StoresWithMissingDays, i)
      MissingNumDays <- c(MissingNumDays, (max(trainStore_i$Date) - min(trainStore_i$Date)) - length(trainStore_i$Date))
    }
  }
}

MissingDays.df <- data.frame(Store = StoresWithMissingDays, Days = MissingNumDays)

# We noticed that all that are missing days have 183 missing days
# For those, we can separate things out