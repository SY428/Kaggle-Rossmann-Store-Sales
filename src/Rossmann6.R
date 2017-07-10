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
testStoreOpen <- subset(testStore, testStore$Open == 1)

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
)
xreg1 <- xreg1[,-1]
colnames(xreg1) <- c("Mon", "Sat", "Thur", "Tues", "Wed", 
                     "Promo", "SchoolHoliday", 
                     "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                     "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday"
)

# "Minimum AIC: -1167.3441648102"
# "Best ARIMA Model: (1,0,5),(3,0,3)"

myARIMA <- Arima(trainStoreOpen1$LogSales, order = c(1,0,5), seasonal = list(order = c(3,0,3), period = 12), xreg = xreg1)

# Predict test store 1
testStore1 <- subset(testStore, testStore$Store == 1)
testStoreOpen1 <- subset(testStoreOpen, testStoreOpen$Store == 1)

newxreg1 <- model.matrix(~ droplevels(as.factor(testStoreOpen1$DayOfWeek)) 
                      + testStoreOpen1$Promo
                      + testStoreOpen1$SchoolHoliday
                      + testStoreOpen1$ClosedDayPrior
                      + testStoreOpen1$ClosingNextDay
                      + testStoreOpen1$PromoWithin3Days
                      + testStoreOpen1$PromoEndedWithin3Days
                      + testStoreOpen1$TimePriorWasSchoolHoliday
                      + testStoreOpen1$DaysWithinSchoolHoliday
)
newxreg1 <- newxreg1[,-1]
colnames(newxreg1) <- c("Mon", "Sat", "Thur", "Tues", "Wed", 
                     "Promo", "SchoolHoliday", 
                     "ClosedDayPrior", "ClosingNextDay", "PromoWithin3Days", 
                     "PromoEndedWithin3Days", "TimePriorWasSchoolHoliday", "DaysWithinSchoolHoliday"
)


pred.date <- as.Date(unique(testStore1$Date))
predOpen.date <- as.Date(unique(testStoreOpen1$Date))
# ggplot(data = subset(trainStoreOpen1, trainStoreOpen1$Date > as.Date("2015-07-01")), aes(Date, LogSales)) + geom_line()

trainSamplePred <- predict(myARIMA, newxreg = newxreg1, n.ahead = length(predOpen.date))
plot2013_range <- ggplot(data = subset(trainStoreOpen1, (trainStoreOpen1$Date >= as.Date("2013-08-01")) & (trainStoreOpen1$Date < as.Date("2013-09-18"))), aes(Date, LogSales)) + geom_line()
plot2014_range <- ggplot(data = subset(trainStoreOpen1, (trainStoreOpen1$Date >= as.Date("2014-08-01")) & (trainStoreOpen1$Date < as.Date("2014-09-18"))), aes(Date, LogSales)) + geom_line()
plot2015_range <- ggplot(data = data.frame(Date = as.Date(predOpen.date), LogSales = trainSamplePred$pred), aes(x = Date, y = LogSales)) + geom_line()
grid.arrange(plot2013_range, plot2014_range, plot2015_range, nrow = 3)

####################################################################################
### Split training data into different groups and train each subgroup separately
####################################################################################

# Different groups use the same arima

# 1. Stores that close regularly on Sunday

# 2. Stores that only close on school or state holidays

# 3. 


