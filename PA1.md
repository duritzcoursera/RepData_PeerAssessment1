# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

```r
library(plyr)
steps.mean <- ddply(activity, .(date), summarize, mean = mean(steps))
library(lattice)
histogram(~mean, steps.mean)
```

![plot of chunk MeanSteps](figure/MeanSteps.png) 

```r
#histogram
```


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
