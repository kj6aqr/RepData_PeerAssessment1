# Reproducible Research: Peer Assessment 1


```r
library(tidyr)
```

## Loading and preprocessing the data

```r
activity<-read.csv("activity.csv")
activity$date<-as.Date(activity$date)
activity_wide<-reshape(activity,timevar="date",idvar=c("interval"),direction="wide")
```

## What is mean total number of steps taken per day?

```r
hist(activity$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean(activity[complete.cases(activity),]$steps)
```

```
## [1] 37.3826
```

```r
median(activity[complete.cases(activity),]$steps)
```

```
## [1] 0
```

## What is the average daily activity pattern?

```r
daily_ave<-data.frame(Interval=activity_wide[,1], AverageSteps=rowMeans(activity_wide[,-1],na.rm=TRUE))
plot(daily_ave,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
daily_ave[which.max(daily_ave$AverageSteps),1]
```

```
## [1] 835
```

## Imputing missing values

```r
sum(is.na(activity_wide))
```

```
## [1] 2304
```

```r
# Find indices of NAs, replace with averages
nas_wide<-which(is.na(activity_wide),arr.ind=TRUE)
activity_wide_nonas<-activity_wide
activity_wide_nonas[nas_wide]<-daily_ave$AverageSteps[nas_wide[,1]]

# Check sums
sum(is.na(activity_wide_nonas))
```

```
## [1] 0
```

```r
# Make long again for easier histograms, etc
activity_wide_long_nonas<-gather(activity_wide_nonas,date,steps,2:dim(activity_wide_nonas)[2])
hist(activity_wide_long_nonas$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
mean(activity_wide_long_nonas[complete.cases(activity),]$steps)
```

```
## [1] 37.3826
```

```r
median(activity_wide_long_nonas[complete.cases(activity),]$steps)
```

```
## [1] 0
```
The mean does not change because the NAs have been replaced by the mean of each value, thereby maintaining the mean.  The median happens to not change, presumably due to the overwhelming number of zeroes.

## Are there differences in activity patterns between weekdays and weekends?

```r
dayOfWeek<-as.factor(weekdays(activity$date))
partOfWeek<-function(day) {
  if(day=="Saturday"||day=="Sunday") {
    return("Weekend")
  } else {
    return("Weekday")
  }
}  
partOfWeek<-sapply(dayOfWeek,partOfWeek)

activity_weekday<-activity[partOfWeek=="Weekday",]
activity_weekend<-activity[partOfWeek=="Weekend",]
activity_weekday_wide<-reshape(activity_weekday,timevar="date",idvar=c("interval"),direction="wide")
activity_weekend_wide<-reshape(activity_weekend,timevar="date",idvar=c("interval"),direction="wide")
weekday_ave<-data.frame(Interval=activity_weekday_wide[,1], AverageSteps=rowMeans(activity_weekday_wide[,-1],na.rm=TRUE))
weekend_ave<-data.frame(Interval=activity_weekend_wide[,1], AverageSteps=rowMeans(activity_weekend_wide[,-1],na.rm=TRUE))

par(mfrow=c(2,1))
plot(weekday_ave,type="l")
title("Weekday Average")
plot(weekend_ave,type="l")
title("Weekend Average")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
