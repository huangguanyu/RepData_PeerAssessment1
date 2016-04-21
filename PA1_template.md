# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data using 'read.csv()'
2. Processes data into suitable formats. e.g. date.


```r
library(knitr)
opts_chunk$set(echo = TRUE)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
unzip("activity.zip")
data <-read.csv("activity.csv")
data$date <- ymd(data$date)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
steps_day <- aggregate(data$steps ~ data$date, FUN=sum, na.rm=TRUE)

colnames(steps_day)<- c("date","steps")
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them.Make a histogram of the total number of steps taken each day.


```r
ghist <- ggplot(steps_day, aes(x=steps_day$steps)) +
          geom_histogram(binwidth = 1000)+ 
          labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")

print(ghist)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean_steps <- mean(steps_day$steps, na.rm=TRUE)

median_steps <- median(steps_day$steps, na.rm=TRUE)

mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,averaged across all days (y-axis)


```r
steps_interval <- aggregate(data$steps ~ data$interval, FUN=mean, na.rm=TRUE)

colnames(steps_interval) <- c("interval", "steps")


gts <- ggplot(steps_interval, aes(x=steps_interval$interval,y=steps_interval$steps)) +
        geom_line(colour="black", size=1.5) +
        labs(title = "Time Series Plot", x = "5-Min Intervals", y = "Average Number of Steps")

print(gts)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
index <-which.max(steps_interval$steps)
max_step_index <- steps_interval$interval[index]
max_step_index
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA).The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing_num<- sum(is.na(data$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. We use mean value of that interval to filling in.

```r
new_data <- data
na_index <- is.na(new_data$steps)
mean_interval <- tapply(new_data$steps, new_data$interval, mean, na.rm=TRUE, simplify=TRUE)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_data$steps[na_index]<- mean_interval[as.character(new_data$interval[na_index])]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
new_steps_day <- aggregate(new_data$steps ~ new_data$date, FUN=sum, na.rm=TRUE)

colnames(new_steps_day)<- c("date","steps")


ghist2 <- ggplot(new_steps_day, aes(x=new_steps_day$steps)) +
  geom_histogram(binwidth = 1000)+ 
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
print(ghist2)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
4. Do these values differ from the estimates from the first part of the assignment?  What is the impact of imputing missing data on the estimates of the total daily number of steps?

Calculate the mean and median steps with the filled in values:


```r
mean_steps_new <- mean(new_steps_day$steps, na.rm= TRUE)

median_steps_new <- median(new_steps_day$steps,na.rm=TRUE)

mean_steps_new
```

```
## [1] 10766.19
```

```r
median_steps_new
```

```
## [1] 10766.19
```

The filled-in and original data are simiar, according to the mean and median values.


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” by using 'weekday()' function.
Determine whether a given date is a weekday or weekend day.

```r
new_data <- mutate(new_data, weektype = ifelse(weekdays(new_data$date) == "Saturday" | weekdays(new_data$date) == "Sunday", "weekend", "weekday"))
new_data$weektype <- as.factor(new_data$weektype)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) andthe average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
weektype_steps <- aggregate(new_data$steps ~new_data$weektype+new_data$interval, FUN=mean, na.rm=TRUE)

colnames(weektype_steps)<- c("weektype","interval","steps")


week_plot<- ggplot(weektype_steps, aes(x=interval,y=steps, color=weektype))+
  geom_line(size=1.5) +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(week_plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

