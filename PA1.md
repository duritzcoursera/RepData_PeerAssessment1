# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
  library(plyr)
  library(lattice)
  unzip("activity.zip")
  activity <- read.csv("activity.csv")
  activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

```r
  activity.clean <- activity[!is.na(activity$steps),]
  steps.day <- ddply(activity.clean, .(date), summarize, total = sum(steps))
  histogram(~total, steps.day)
```

![plot of chunk DailyActivity](figure/DailyActivity.png) 

```r
  mean(steps.day$total)
```

```
## [1] 10766
```

```r
  median(steps.day$total)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
  steps.interval <- ddply(activity.clean, .(interval), summarize, mean = mean(steps))
  xyplot(mean~interval, data=steps.interval, type="l")
```

![plot of chunk IntervalActivity](figure/IntervalActivity.png) 

```r
  steps.interval.max <- steps.interval$interval[which.max(steps.interval$mean)]
  steps.interval.max
```

```
## [1] 835
```


## Imputing missing values

```r
  missing <- activity[is.na(activity$steps),]
  missing.index <- is.na(activity$steps)
  missing.count <- nrow(missing)
  missing.count
```

```
## [1] 2304
```

```r
  activity.imp <- activity
  activity.imp$steps[missing.index] <- unlist(sapply(activity$interval[missing.index], function(x) {steps.interval$mean[steps.interval$interval == x]}))
  steps.day.imp <- ddply(activity.imp, .(date), summarize, total = sum(steps))
  histogram(~total, steps.day.imp)
```

![plot of chunk ImputeMissingValues](figure/ImputeMissingValues.png) 

```r
  mean(steps.day.imp$total)
```

```
## [1] 10766
```

```r
  median(steps.day.imp$total)
```

```
## [1] 10766
```
The median differs slightly from before because we added more entries using the mean steps in an interval.

## Are there differences in activity patterns between weekdays and weekends?
