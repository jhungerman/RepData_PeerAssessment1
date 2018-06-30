#load packages
library(knitr)
library(dplyr)
library(xtable)
library(ggplot2)
library(tidyr)
#import data
activity <- read.csv(file="activity.csv", header = TRUE, stringsAsFactors = FALSE)
#change the date variable from character to date
activity$date <- as.Date(activity$date, "%Y-%m-%d")
#aggregate and sum to achieve total steps per day
stepsByDay <- aggregate(steps ~ date, activity, sum)
#create and print a histogram of steps per day
stepHist <- ggplot(stepsByDay, aes(steps)) + geom_histogram(binwidth = 2500) + labs(title = "Total Steps per Day",x = "Steps", y = "Number of Days") + theme(plot.title = element_text(hjust=0.5))
print(stepHist)
#calculate the mean and median of steps per day and print to the console
meanSteps <- mean(stepsByDay$steps)
medSteps <- median(stepsByDay$steps)
cat("Mean steps per day: ", meanSteps)
cat("\nMedian steps per day: ", medSteps)
#aggrefate and average to get average steps per 5 minute interval
avgInt <- aggregate(steps ~ interval, activity, mean)
#create and print a line time series plot to show average steps over 5 minute intervals
intLine <- ggplot(avgInt, aes(x=interval, y=steps)) + geom_line() + geom_point() + labs(title = "Time Series of Average Steps per 5 Minute Interval",x = "5 Minute Interval",y = "Average Steps") + theme(plot.title = element_text(hjust=0.5))
print(intLine)
#find the maximum average steps for a 5 minute interval, strip the values into new variables, print the result to the console
maxInt <- avgInt[which(avgInt$steps==max(avgInt$steps)),]
maxIntRge <- maxInt$interval
maxIntVal <- maxInt$steps
cat("\nMaximum Interval and Steps: ", maxIntRge, " - ", maxIntVal)
