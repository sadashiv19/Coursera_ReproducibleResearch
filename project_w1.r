library(ggplot2)

## setting working directory
path <- "D:/Coursera/ReproducibleResearch/"   
setwd(path)
unzip(zipfile="repdata-data-activity.zip")
activityData <- read.csv("activity.csv")

## Histogram of the total number of steps taken each day
sumOfSteps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
qplot(sumOfSteps, binwidth=1000, xlab="total number of steps taken each day")


## Mean and median number of steps taken each day
mean_sumOfSteps <- mean(sumOfSteps, na.rm=TRUE)
median_sumOfSteps <- median(sumOfSteps, na.rm=TRUE)
paste(mean_sumOfSteps)
paste(median_sumOfSteps)

## Time series plot of the average number of steps taken
average <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("avg number of steps taken")

## The 5-minute interval that, on average, contains the maximum number of steps
averages[which.max(average$steps),]

## imputing missing data
table(is.na(activityData$steps) )

## function to populate NA values
fill_val <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}

activityData$steps <- mapply(fill_val, activityData$steps, activityData$interval)


## Histogram of the total number of steps taken each day after missing values are imputed
sumOfSteps_imputed <- tapply(activityData$steps, activityData$date, FUN=sum)
qplot(sumOfSteps_imputed, binwidth=1000, xlab="total number of steps taken each day")

sumOfSteps_imputed_mean <- mean(sumOfSteps_imputed)
sumOfSteps_imputed_median <- median(sumOfSteps_imputed)

paste(sumOfSteps_imputed_mean)
paste(sumOfSteps_imputed_median)


## Panel plot comparing the average number of steps taken per 5-minute interval across 
## weekdays and weekends

weekday_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

activityData$date <- as.Date(activityData$date)
activityData$day <- sapply(activityData$date, FUN=weekday_weekend)
aveg2 <- aggregate(steps ~ interval + day, data=activityData, mean)

ggplot(aveg2, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
