#load packages
library(knitr)
library(dplyr)
library(xtable)
library(ggplot2)
library(tidyr)
#import data
activity <- read.csv(file="activity.csv", header = TRUE, stringsAsFactors = FALSE)
