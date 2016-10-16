
#library(lubridate)
library(plyr)
library(ggplot2)


mydata = read.csv("activity.csv", header = TRUE)
mydata$date = as.Date(mydata$date, format= "%Y-%m-%d")

#split data by date and compute total number of steps taken each day:
totalSteps = ddply(mydata, .(date), summarize, tot = sum(steps))

myplot = qplot(totalSteps$tot, geom = "histogram")

#calculate teh mean and median number of steps taken each day:
mymean = mean(totalSteps$tot, na.rm = TRUE)
mymedian = median(totalSteps$tot, na.rm = TRUE)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all days (y-axis)

avgSteps = ddply(mydata, .(interval), summarize, aveSteps = mean(steps, na.rm = TRUE))
plot(avgSteps$interval, avgSteps$aveSteps, type = 'l')


#The 5-minute interval that, on average, contains the maximum number of steps
avgSteps$interval[which(avgSteps$aveSteps == max(avgSteps$aveSteps, na.rm = TRUE))]

##Imputing missing values

#1. Calculate and report the total number of missing values in the dataset
#(i.e. the total number of rows with NAs)
#the function complete.cases returns a logical vector indicating if there is no NA in that row (TRUE) or FALSE otherwise. The
#length function tells us how many rows have NAs
numRowsWithNAs = length(which(complete.cases(mydata) == FALSE))

# Devise a strategy for filling in all of the missing values in the dataset. The
# strategy does not need to be sophisticated. For example, you could use
# the mean/median for that day, or the mean for that 5-minute interval, etc.

#Upon further inspection, the only column with missing values is the one for the number of steps per day
length(which(complete.cases(mydata$steps) == FALSE))
length(which(complete.cases(mydata$interval) == FALSE))
length(which(complete.cases(mydata$date) == FALSE))

#So we will input the missing values with the average number of steps for that day
# newdata <- ddply(mydata, .(date), function(x)
#   if x$steps == NA:
#     x$steps = mean(x$steps, na.rm = TRUE)
#     )

library(dplyr)
library(magrittr)
#add a column to the old data set to compute the mean number of steps in each interval 
mydata = mydata%>% group_by(interval) %>% mutate(mean_steps = mean(steps, na.rm = T))
# data$new_steps = steps #with(data, ifelse(is.na(steps), mean_steps, steps))

#create a new data set that is exactly the same as the old one but with the missing values filled in with the mean of the 
#steps for that interval
newdata = mydata[, 1:3]
# temp =which(is.na(mydata$steps) == TRUE)
newdata$steps[which(is.na(newdata$steps) == TRUE)]<- mydata$mean_steps[which(is.na(mydata$steps) == TRUE)]


#Make a histogram with the total number of steps 
totalSteps2 = ddply(newdata, .(date), summarize, tot = sum(steps))

myplot2 = qplot(totalSteps2$tot, geom = "histogram")

#calculate teh mean and median number of steps taken each day:
mymean2 = mean(totalSteps2$tot)
mymedian2 = median(totalSteps2$tot)


#create a column that identifies weekends and weekdays
# newdata$wkday = weekdays(newdata$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
newdata$wDay <- c('weekend', 'weekday')[(weekdays(newdata$date) %in% weekdays1)+1L]
newdata = newdata%>% group_by(interval, wDay) %>% mutate(mean_steps = mean(steps, na.rm = T))

avgSteps = ddply(newdata, .(interval, wDay), summarize, aveSteps = mean(steps))
newplot <- ggplot(avgSteps, aes(x = interval, y= aveSteps))+ geom_line()+ facet_grid(wDay ~ .)


