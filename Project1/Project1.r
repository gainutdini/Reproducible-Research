# Reproducible Research: Course Project 1

## Loading and reading the data
data <- read.csv("activity.csv", header=TRUE)

# 1. Subset the original data to calculate the total number of steps
steps_bDay <- aggregate(steps~date, data, sum)
head(steps_bDay)

# 2. Make a histogram of the total number of steps taken each day
hist(steps_bDay$steps, col = "Darkgreen"
     , xlab = "Steps", main = "Total Number of Steps Per Day")

# 3. Calculate the mean and median number of steps taken each day
mean_steps <- mean(stepsByDay$steps)
median_steps <- median(stepsByDay$steps)

# 4. Subset the original data to calculate the average number of steps taken by interval
avg_bDay <- aggregate(steps~interval, data, mean)
with(avg_bDay, plot(interval,steps,col = "Darkblue", type = "l", 
                    xlab = "Interval" ,ylab="Average number of steps", 
                    main = "Time series plot of the average number of steps taken"))

# 5. Finding the  interval that, on average, contains the maximum number of steps
int_max_steps <- avg_bDay$interval[max(avg_bDay$steps)]


# 6. Imputing missing values
# 6.1 Calculate and report the total number of missing values in the dataset

na_rows <-  sum(is.na(data$steps))

# 6.2 Make a new column with mean of steps taken each day
data$newSteps <- avg_bDay$steps

# 6.3 Fill the NA's in steps
data$steps[is.na(data$steps)] <- data$newSteps
#remove the added column
data$newSteps <- NULL

# 7. Make a histogram of the total number of steps taken each day
steps_new <- aggregate(steps~date, data, sum)
hist(steps_new$steps,col = "Darkblue",
     xlab = "Steps", main = "Total Number of Steps Per Day after filling Na's")

# 7.1
mean_new <- mean(stepsByDay$steps)
median_new <- median(stepsByDay$steps)

## Are there differences in activity patterns between weekdays and weekends?
#load the timeDate package
# install.packages("timeDate")
library(timeDate)

#2. Create a new column, and use the isWeekday() function to check if the date is weekday or weekend
data$Weekday <- isWeekday(data$date)
head(newdata)

#3. Subset and calculate the average steps for weekday and weekend
weekday <- subset(data, data$Weekday == "TRUE")
weekdayMean <- aggregate(steps ~ interval, data = weekday, mean)
head(weekdayMean)

weekend <- subset(data, data$Weekday == "FALSE")
weekendMean <- aggregate(steps ~ interval, data = weekend, mean)
head(weekendMean)

#4. Make the panel plot to calculate the average number of steps taken for weekday and weekend
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
plot(weekdayMean$interval, weekdayMean$steps
     , xlab = "interval", ylab = "Number of steps"
     , main ="Weekday", col ="blue", type="l") 

plot(weekendMean$interval, weekendMean$steps
     , xlab = "interval", ylab = "Number of steps"
     , main ="Weekend", col ="red", type="l")







