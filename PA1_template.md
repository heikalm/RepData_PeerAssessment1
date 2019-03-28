---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
# Check for file, download and unzip


```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
filename <- 'reproducibleresearchassignment1'
fileURL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'

if (!file.exists(filename)){
        download.file(fileURL, filename, method="curl")
        unzip(filename)
}  


#read into dataframe

activitydata <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?

```r
# histogram, mean and median of daily sum of steps
dailysum <- aggregate(steps ~ date, activitydata, sum)
hist(dailysum$steps, main='Histogram of steps in a day (imputed NAs)', xlab = 'Daily steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
mean(dailysum$steps)
```

```
## [1] 10766.19
```

```r
median(dailysum$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# average steps per interval
intervalmean <- aggregate(steps ~ interval, activitydata, mean)
qplot(data=intervalmean, x=interval, y=steps, geom='line')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# which interval has the largest average number of steps
intervalmean[which.max(intervalmean$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
# total number of NAs
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```

```r
# replace NAs with mean value for that interval
navalues <- is.na(activitydata$steps)

# replace NAs with interval mean
# Step 1: create a column of interval means for every interval in the original data
mergeddata <- left_join(activitydata, intervalmean, by = "interval")

# Step 2: replace NAs in steps with the corresponding interval mean
mergeddata$steps.x[navalues] <- mergeddata$steps.y[navalues]

# Step 3: remove interval mean column and rename steps column
filledactivitydata <- mergeddata[1:3]
names(filledactivitydata)[1] <- 'steps'

# histogram, mean and median of daily sum of steps using filled data
filleddailysum <- aggregate(steps ~ date, filledactivitydata, sum)
hist(filleddailysum$steps, main='Histogram of steps in a day (imputed NAs)', xlab = 'Daily steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(filleddailysum$steps)
```

```
## [1] 10766.19
```

```r
median(filleddailysum$steps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

```r
# day of week for each date
filledactivitydata$dayofweek <- weekdays(as.Date(filledactivitydata$date))

# rename weekdays to "weekday" and weekends to "weekend"
weekends <- filledactivitydata$dayofweek %in% c("Saturday", "Sunday")
filledactivitydata[weekends,]$dayofweek <- "weekend"
filledactivitydata[!weekends,]$dayofweek <- "weekday"
filledactivitydata$dayofweek <- as.factor(filledactivitydata$dayofweek)

#panel plot
g <- ggplot(data=filledactivitydata, aes(x=interval, y=steps))
g + 
        stat_summary(fun.y='mean', geom='line') + 
        facet_grid(dayofweek ~ . ) +
        labs(title = 'Average steps per interval, weekday vs weekend')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
