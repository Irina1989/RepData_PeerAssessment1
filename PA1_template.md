# Reproducible Research: Peer Assessment 1

library(plyr)
library(lattice)
## Loading and preprocessing the data

```r
data <- read.csv('activity.csv')

# check if data looks plausible
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

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
dim(data)
```

```
## [1] 17568     3
```

```r
# transform the variable date from factor to datetime format
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

```r
# calculate and plot total number of steps per day
steps_per_day <- aggregate(steps ~ date, data = data, FUN = sum)
hist(steps_per_day$steps, breaks = 10, 
     main = "Total Number of Steps per Day",
     xlab = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)\

```r
# mean and median of total number of steps per day
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# average steps per interval
steps_per_interval <- aggregate(steps ~ interval, data = data, FUN = mean)

# plot average per interval
plot(steps_per_interval$interval, steps_per_interval$steps, type = 'l',
     xlab = "Interval", main = "average no. of steps during a day",
     ylab = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\

```r
# interval with on average maximum number of steps
steps_per_interval[which.max(steps_per_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
# number of rows with missing data
dim(data[!complete.cases(data),])[1]
```

```
## [1] 2304
```

```r
# imputing missing values and creating new dataset
library(plyr)
data_imputed = data
imputer <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)) 
data_imputed <- ddply(data_imputed, ~ interval, transform, steps = imputer(steps)) 
# calculate and plot total number of steps per day of imputed data
steps_per_day_imputed <- aggregate(steps ~ date, data = data_imputed, FUN = sum)
hist(steps_per_day_imputed$steps, breaks = 10, 
     main = "Total Number of Steps per Day (imputed)",
     xlab = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

```r
# mean and median of total number of steps per day
mean(steps_per_day_imputed$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_imputed$steps)
```

```
## [1] 10766.19
```

```r
# Imputing missing data only has a really small effect, only the median changes slightly
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# create new variable indicating weekday or weekend
data_imputed$weekday <- as.factor(ifelse(weekdays(data_imputed$date) %in% c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"), "weekday", "weekend"))

# plot groups
weekday.average <- aggregate(steps ~ interval + weekday, data = data_imputed, FUN = mean)

library(lattice)
xyplot(steps ~ interval | weekday, data = weekday.average, type = "l", layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)\

