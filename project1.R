#this program is for reproducible research project 1

library(knitr)
opts_chunk$set(echo = TRUE)

library(dplyr)
library(lubridate)
library(ggplot2)

# Loading and preprocessing the data
unzip("activity.zip")
data <-read.csv("activity.csv")
# tidy date data
data$date <- ymd(data$date)


## What is mean total number of steps taken per day?
##1,Calculate the total number of steps taken per day
steps_day <- aggregate(data$steps ~ data$date, FUN=sum, na.rm=TRUE)

colnames(steps_day)<- c("date","steps")

##2,If you do not understand the difference between a histogram and a barplot, research the difference between them. 
##Make a histogram of the total number of steps taken each day

png("plot_1.png")
ghist <- ggplot(steps_day, aes(x=steps_day$steps)) +
          geom_histogram(binwidth = 1000)+ 
          labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")

print(ghist)
dev.off()
##3,Calculate and report the mean and median of the total number of steps taken per day

mean_steps <- mean(steps_day$steps, na.rm=TRUE)

median_steps <- median(steps_day$steps, na.rm=TRUE)



## What is the average daily activity pattern?

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
##averaged across all days (y-axis)

steps_interval <- aggregate(data$steps ~ data$interval, FUN=mean, na.rm=TRUE)

colnames(steps_interval) <- c("interval", "steps")

png('plot_2.png')
gts <- ggplot(steps_interval, aes(x=steps_interval$interval,y=steps_interval$steps)) +
        geom_line(colour="black", size=1.5) +
        labs(title = "Time Series Plot", x = "5-Min Intervals", y = "Average Number of Steps")

print(gts)
dev.off()

        
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
index <-which.max(steps_interval$steps)
max_step_index <- steps_interval$interval[index]

## Imputing missing values
##Note that there are a number of days/intervals where there are missing values (coded as NA). 
##The presence of missing days may introduce bias into some calculations or summaries of the data.

##1,Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing_num<- sum(is.na(data$steps))

##Devise a strategy for filling in all of the missing values in the dataset.
##The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
##or the mean for that 5-minute interval, etc.

## We use mean value of that interval to filling in.
new_data <- data
na_index <- is.na(new_data$steps)
mean_interval <- tapply(new_data$steps, new_data$interval, mean, na.rm=TRUE, simplify=TRUE)


##Create a new dataset that is equal to the original dataset but with the missing data filled in.
new_data$steps[na_index]<- mean_interval[as.character(new_data$interval[na_index])]

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
new_steps_day <- aggregate(new_data$steps ~ new_data$date, FUN=sum, na.rm=TRUE)

colnames(new_steps_day)<- c("date","steps")

png("plot_3.png")
ghist2 <- ggplot(new_steps_day, aes(x=new_steps_day$steps)) +
  geom_histogram(binwidth = 1000)+ 
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
print(ghist2)
dev.off()

##Do these values differ from the estimates from the first part of the assignment? 
##What is the impact of imputing missing data on the estimates of the total daily number of steps?
##Calculate the mean and median steps with the filled in values:

mean_steps_new <- mean(new_steps_day$steps, na.rm= TRUE)

median_steps_new <- median(new_steps_day$steps,na.rm=TRUE)


## Are there differences in activity patterns between weekdays and weekends?
##1,For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
##indicating whether a given date is a weekday or weekend day.
new_data <- mutate(new_data, weektype = ifelse(weekdays(new_data$date) == "Saturday" | weekdays(new_data$date) == "Sunday", "weekend", "weekday"))
new_data$weektype <- as.factor(new_data$weektype)


##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
##the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

weektype_steps <- aggregate(new_data$steps ~new_data$weektype+new_data$interval, FUN=mean, na.rm=TRUE)

colnames(weektype_steps)<- c("weektype","interval","steps")

##plot
png("plot_4.png")
week_plot<- ggplot(weektype_steps, aes(x=interval,y=steps, color=weektype))+
  geom_line(size=1.5) +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(week_plot)
dev.off()




