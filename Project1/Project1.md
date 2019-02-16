---
title: 'Reproducible Research: Peer Assessment 1'
author: "NiazG"
output: word_document
---

## Loading and preprocessing the data

1. Load the data:

```r
data <- read.csv("activity.csv", header=TRUE)
```

2. Show the original data:

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

1. Subset the original data to calculate the total number of steps

```r
steps_bDay <- aggregate(steps~date, data, sum)
head(steps_bDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. Make a histogram of the total number of steps taken each day


```r
hist(steps_bDay$steps, col = "Darkgreen"
     , xlab = "Steps", main = "Total Number of Steps Per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3. Calculate the mean and median number of steps taken each day

```r
mean_steps <- mean(steps_bDay$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps <- median(steps_bDay$steps)
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Subset the original data to calculate the average number of steps taken by interval


```r
avg_bDay <- aggregate(steps~interval, data, mean)
head(avg_bDay)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

2. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
with(avg_bDay, plot(interval,steps,col = "Darkblue", type = "l", 
                    xlab = "Interval" ,ylab="Average number of steps", 
                    main = "Time series plot of the average number of steps taken"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

3. Finding the  interval that, on average, contains the maximum number of steps

```r
int_max_steps <- avg_bDay$interval[max(avg_bDay$steps)]
int_max_steps
```

```
## [1] 1705
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's)

```r
na_rows <-  sum(is.na(data$steps))
na_rows
```

```
## [1] 2304
```

2. Make a new column with mean of steps taken each day

```r
data$newSteps <- avg_bDay$steps
head(data)
```

```
##   steps       date interval  newSteps
## 1    NA 2012-10-01        0 1.7169811
## 2    NA 2012-10-01        5 0.3396226
## 3    NA 2012-10-01       10 0.1320755
## 4    NA 2012-10-01       15 0.1509434
## 5    NA 2012-10-01       20 0.0754717
## 6    NA 2012-10-01       25 2.0943396
```

3. Fill the NA's in steps

```r
data$steps[is.na(data$steps)] <- data$newSteps
```

```
## Warning in data$steps[is.na(data$steps)] <- data$newSteps: number of items
## to replace is not a multiple of replacement length
```

```r
head(data)
```

```
##       steps       date interval  newSteps
## 1 1.7169811 2012-10-01        0 1.7169811
## 2 0.3396226 2012-10-01        5 0.3396226
## 3 0.1320755 2012-10-01       10 0.1320755
## 4 0.1509434 2012-10-01       15 0.1509434
## 5 0.0754717 2012-10-01       20 0.0754717
## 6 2.0943396 2012-10-01       25 2.0943396
```

4. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
data$newSteps <- NULL
newdata <- data
```

5. Subset in the new data set to calculate the total number of steps per day

```r
steps_new <- aggregate(steps~date, newdata, sum)
head(steps_new)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

6. Make a histogram of the total number of steps taken each day

```r
hist(steps_new$steps,col = "Darkblue",
     xlab = "Steps", 
     main = "Total Number of Steps Per Day after filling Na's")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)


7. Calculate the differences of the mean and median between the first and second part

```r
mean_new <- mean(steps_new$steps)
mean_new
```

```
## [1] 10766.19
```

```r
median_new <- median(steps_new$steps)
median_new
```

```
## [1] 10766.19
```

8. Calculate the differences of the mean and median between the first and second part

```r
mean_steps - mean_new
```

```
## [1] 0
```

```r
median_steps - median_new
```

```
## [1] -1.188679
```

## Are there differences in activity patterns between weekdays and weekends?
For this part, I do not use the weekdays() function, instead I use the isWeekday() function from the timeDate package.

1. Install and load the timeDate package
install.packages("timeDate")

```r
library(timeDate)
```

2. Create a new column, and use the isWeekday() function to check if the date is weekday or weekend

```r
newdata$Weekday <- isWeekday(newdata$date)
head(newdata)
```

```
##       steps       date interval Weekday
## 1 1.7169811 2012-10-01        0    TRUE
## 2 0.3396226 2012-10-01        5    TRUE
## 3 0.1320755 2012-10-01       10    TRUE
## 4 0.1509434 2012-10-01       15    TRUE
## 5 0.0754717 2012-10-01       20    TRUE
## 6 2.0943396 2012-10-01       25    TRUE
```

3. Subset and calculate the average steps for weekday and weekend

Weekday

```r
weekday <- subset(newdata, newdata$Weekday == "TRUE")
weekdayMean <- aggregate(steps ~ interval, data = weekday, mean)
head(weekdayMean)
```

```
##   interval      steps
## 1        0 2.25115304
## 2        5 0.44528302
## 3       10 0.17316562
## 4       15 0.19790356
## 5       20 0.09895178
## 6       25 1.59035639
```

Weekend

```r
weekend <- subset(newdata, newdata$Weekday == "FALSE")
weekendMean <- aggregate(steps ~ interval, data = weekend, mean)
head(weekendMean)
```

```
##   interval       steps
## 1        0 0.214622642
## 2        5 0.042452830
## 3       10 0.016509434
## 4       15 0.018867925
## 5       20 0.009433962
## 6       25 3.511792453
```

4. Make the panel plot to calculate the average number of steps taken for weekday and weekend

```r
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
plot(weekdayMean$interval, weekdayMean$steps
     , xlab = "interval", ylab = "Number of steps"
     , main ="Weekday", col ="blue", type="l") 
plot(weekendMean$interval, weekendMean$steps
     , xlab = "interval", ylab = "Number of steps"
     , main ="Weekend", col ="red", type="l")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)


















