---
title: "Course Project 1 - Reproducible Research"
author: "Mike"
date: "11/16/2021"
output: 
  html_document: 
    keep_md: yes
---



```r
# Set working directory for file directory
# Load the data (i.e. read.csv(), read.csv())
fulldata<-read.table("activity.csv", header=TRUE, sep=",", na.strings="NA")
```

# Process/transform the data (if necessary) into a format suitable for your analysis

```r
cleandata<-fulldata
cleandata$interval<-as.factor(cleandata$interval)
cleandata$steps<-as.numeric(cleandata$steps)
cleandata<-cleandata[complete.cases(cleandata),]
```

# Calculate the total number of steps taken per day

```r
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
stepsxday<-cleandata%>%group_by(date)%>%summarize(sum(steps))

# Make a histogram of the total number of steps taken each day
hist(stepsxday$`sum(steps)`,main="Histogram of Total # of Steps Each Day", xlab="Sum Steps")
```

![](CourseProj1_files/figure-html/TotalStepsPerDay-1.png)<!-- -->

```r
# Calculate and report the mean and median of the total number of steps taken per day
summary(stepsxday$`sum(steps)`)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

# Make a time series plot (i.e.,type = "l") of the 5-minute interval (x-axis)...
# and the average number of steps taken, averaged across all days (y-axis)

```r
stepsxinterval<-cleandata%>%group_by(interval)%>%summarize(mean(steps))
plot(stepsxinterval$interval, stepsxinterval$`mean(steps)`, type="l", xlab="avg steps", ylab="interval")
```

![](CourseProj1_files/figure-html/StepsPerIntervals-1.png)<!-- -->

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxinterval<-which.max(stepsxinterval$`mean(steps)`)
stepsxinterval[maxinterval,]
```

```
## # A tibble: 1 x 2
##   interval `mean(steps)`
##   <fct>            <dbl>
## 1 835               206.
```

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(fulldata))
```

```
## [1] 2304
```

```r
# Devise a strategy for filling in all of the missing values in the dataset.
# (replace NA values with '0')
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
newdata<-fulldata
newdata[is.na(newdata)]<-0
```

# Make a histogram of the total number of steps taken each day... 

```r
newstepsxday<-newdata%>%group_by(date)%>%summarize(sum(steps))
hist(newstepsxday$`sum(steps)`,main="Histogram of Total # of Steps Each Day", xlab="Sum Steps")
```

![](CourseProj1_files/figure-html/newStepsPerDay-1.png)<!-- -->

```r
# Calculate and report the mean and median total number of steps taken per day (for new data)...
summary(newstepsxday$`sum(steps)`)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

# Do these values differ from the estimates from the first part of the assignment?
# ANSWER:  YES

# What is the impact of imputing missing data on the estimates of the total daily number of steps?
# ANSWER:  Replacing NA values with zero: decreased the minimum value, mean and median; increased the range.

# Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend"...
# indicating whether a given date is a weekday or weekend day.

```r
weekdata<-newdata
weekdata$date<-as.Date(weekdata$date)
weekdata$date<-weekdays(weekdata$date)
weekdata$date<-as.character(weekdata$date)

weekdata <- mutate(weekdata, weekCAT=weekdata$date)
weekdata$weekCAT[weekdata$weekCAT=="Saturday"|weekdata$weekCAT=="Sunday"]<-"weekend"
weekdata$weekCAT[weekdata$weekCAT!="weekend"]<-"weekday"
weekdata$interval<-as.factor(weekdata$interval)
```

#weekdata$weekCAT<-as.factor(weekdata$weekCAT)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and...
# the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
weekENDsteps<-subset(weekdata, weekCAT=="weekend")
weekDAYsteps<-subset(weekdata, weekCAT=="weekday")

stepsxEND<-weekENDsteps%>%group_by(interval)%>%summarize(mean(steps))
stepsxDAY<-weekDAYsteps%>%group_by(interval)%>%summarize(mean(steps))

par(mfrow=c(2,1))
plot(stepsxEND$interval, stepsxEND$`mean(steps)`, type="l", xlab="avg steps", ylab="interval",main="weekend")
plot(stepsxDAY$interval, stepsxDAY$`mean(steps)`, type="l", xlab="avg steps", ylab="interval",main="weekday")
```

![](CourseProj1_files/figure-html/weekCAT-1.png)<!-- -->
