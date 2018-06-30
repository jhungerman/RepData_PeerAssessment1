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
