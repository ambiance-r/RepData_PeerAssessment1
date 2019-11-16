## Reproducible Research - Peer-graded Assignment: Course Project 1


## Set the path and load the necessary packages:
setwd("C:/Users/user/datasciencecoursera/RepData_PeerAssessment1")
library(dplyr)
library(ggplot2)

filename <- "activity.zip"

## Check if the data folder has already been downloaded. If not, download:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, filename, method="curl")
}  

## Check if the (zipped) folder has already been unzipped. If not, unzip:
if (!file.exists("activity")) { 
  unzip(filename) 
}

## 1. Load the data and (if needed) transform into a suitable format for the analysis:
data <- read.csv("activity.csv", header = TRUE)

## Total number of steps taken each day
datadaily <- data %>% group_by(date) %>% summarise(sum(steps))
names(datadaily)[2] <- "Total steps"

## 2. Histogram of the total number of steps taken each day:
png('datadaily.png', width=480, height=480)
hist(datadaily$`Total steps`, main="Total number of steps taken each day", xlab="Steps per day", ylab="Frequency", col="darkmagenta")
dev.off()

## 3. Mean and median number of steps taken each day:
meanstep <- mean(datadaily$`Total steps`, na.rm = TRUE)
print(meanstep)
medianstep <- median(datadaily$`Total steps`, na.rm = TRUE)
print(medianstep)

## 4. Time series plot of the average number of steps taken:
avgstepsgrouped1 <- data %>% group_by(interval) %>% summarise(mean(steps, na.rm = TRUE))
names(avgstepsgrouped1)[2] <- "Average steps"

## Generate the plot and save as a .png file:
png('avgsteps.png', width=480, height=480)
g <- ggplot(avgstepsgrouped1, aes(avgstepsgrouped1$interval, avgstepsgrouped1$`Average steps`))
g + geom_line(col="steelblue", na.rm = TRUE) + labs(title = "Average number of steps taken per 5-minute interval") + labs(x = "Interval") + labs(y = "Number of steps")
dev.off()

## 5. The 5-minute interval that, on average, contains the maximum number of steps:
maxstep <- max(avgstepsgrouped1$`Average steps`)
intervalmaxwithdate <- avgstepsgrouped1[which(avgstepsgrouped1$`Average steps` == maxstep),1:2]
print(intervalmaxwithdate)

## Calculate and report the total number of missing values in the dataset (i.e. the total 
## number of rows with NAs)
sum(is.na(data$steps))

## 6. Code to describe and show a strategy for imputing missing data. 

## Here we will create a new dataset that is equal to the original dataset but with the missing 
## data filled in.

rownr <- dim(data)[1]
datena <- as.data.frame(data$date)
names(datena)[1] <- "datena"        ## dates with NAs
dataimputed <- data

## Mean number of steps per day:
meansteps <- data %>% group_by(date) %>% summarise(mean(steps))
names(meansteps)[2] <- "Average mean steps"

## Missing values are filled with meansteps values. If meansteps value is also missing, then, they are
## filled with the mean of meansteps:

for (i in 1:rownr){
  if (is.na(data[i, 1]) == TRUE & is.na(meansteps[which(meansteps$date == datena[i,1]),2]) == TRUE){
      dataimputed[i, 1] <- mean(meansteps$`Average mean steps`, na.rm=TRUE)
  } else if (is.na(data[i, 1]) == TRUE & is.na(meansteps[which(meansteps$date == datena[i,1]),2]) == FALSE){
    dataimputed[i, 1] <- meansteps[which(meansteps$date == datena[i,1]),2]
  } else if (is.na(data[i, 1]) == FALSE){
  dataimputed[i, 1] <- data[i,1]
  }
}  

## 7. Histogram of the total number of steps taken each day after missing values are imputed:
datadailyimputed <- dataimputed %>% group_by(date) %>% summarise(sum(steps))
names(datadailyimputed)[2] <- "Total steps"

png('datadailyimputed.png', width=480, height=480)
hist(datadailyimputed$`Total steps`, main="Total number of steps taken each day", xlab="Steps per day", ylab="Frequency", col="darkmagenta")
dev.off()

## The mean and median total number of steps taken per day. Do these values differ from the 
## estimates from the first part of the assignment? What is the impact of imputing missing data 
## on the estimates of the total daily number of steps?
meanstepimputed <- mean(datadailyimputed$`Total steps`, na.rm = TRUE)
print(meanstepimputed)
medianstepimputed <- median(datadailyimputed$`Total steps`, na.rm = TRUE)
print(medianstepimputed)

## These values differ from the estimates from the first part of the assignment. The estimates of 
## the total daily number of steps are now higher as we've now inserted positive values in place of
## the NA values, which were converted to 0's while calculating the mean and the median.

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays
## and weekends. Create a new factor variable in the dataset with two levels - "weekday" and 
## "weekend" indicating whether a given date is a weekday or weekend day.

dataimputed$day <- weekdays(as.Date(dataimputed$date))
for (i in 1:rownr) {
  if (dataimputed[i,4] %in% c("Saturday","Sunday")){
    dataimputed$dayofweek[i] <- "weekend"
  } else if (dataimputed[i,4] %in% c("Monday","Tuesday", "Wednesday", "Thursday", "Friday")) {
    dataimputed$dayofweek[i] <- "weekday"
  }
}
 
avgstepsgrouped <- dataimputed %>% group_by(interval, dayofweek) %>% summarise(mean(steps))
names(avgstepsgrouped)[3] <- "Average steps"

## Generate the plot and save as a .png file:
png('avgstepsimputed.png', width=480, height=480)
g <- ggplot(avgstepsgrouped, aes(avgstepsgrouped$interval, avgstepsgrouped$`Average steps`))
g + geom_line(col="steelblue") + facet_grid(. ~ avgstepsgrouped$dayofweek) + labs(title = "Average number of steps taken per 5-minute interval") + labs(x = "Interval") + labs(y = "Number of steps")
dev.off()
