# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data  

First unzip the file and read the contained csv

```r
unzipedFilename<-unzip("activity.zip")
activityDF<-read.csv(unzipedFilename)
```

Look into the file

```r
head(activityDF)
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
str(activityDF)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Set the "date" variable to the proper R Date format and convert the time to a 4 digit basically padding with 0s where required.


```r
library(stringr)
activityDF$date<-as.Date(activityDF$date)
#padding with 0s the left side of the times to reach length 4.
activityDF$date.time<-(strptime(paste(activityDF$date, str_pad(string=as.character(activityDF$interval), width = 4, side = "left", pad = "0")), "%Y-%m-%d %H%M"))
str(activityDF)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps    : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date     : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date.time: POSIXlt, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
```

## What is mean total number of steps taken per day?

Missing values are ignored

Getting all the steps done per day

```r
total<-tapply(activityDF$steps, activityDF$date, sum, na.rm = TRUE)
```
Histogram is made

```r
hist(total, breaks = 10, main = "Histogram of steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

Mean:

```r
mean(total)
```

```
## [1] 9354.23
```
Median:

```r
median(total)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Computing the daily pattern putting time in the x axis.

```r
times<-unique(strftime(activityDF$date.time, format = "%T"))
steps<-tapply(activityDF$steps, activityDF$interval, sum, na.rm = TRUE)
plot(steps, type = "l", xaxt = "n", xlab = "Time")
axis(1, at = 1:288, labels = times)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)
Plot with interval in the x axis.

```r
plot(unique(activityDF$interval), steps, type = "l", xlab = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)

Computing the maximum number of steps and the corresponding interval.

```r
maxSteps<-max(steps)
indexMax<-names(which(steps == max(steps)))
```
The maximum number of steps is 10927 and is achieved at index 835.


In the time domain the maximum number of steps is achieved at time:

```r
timeStepsDF<-data.frame(times,steps)
timeStepsDF[timeStepsDF$steps == max(timeStepsDF$steps),]
```

```
##        times steps
## 835 08:35:00 10927
```


## Imputing missing values

Computation of the total number of missing values.

```r
numNas<-sum(is.na(activityDF$steps))
```

The number of missing values is 2304.


Fixing the missing values with the median of the steps in the same 5-minutes interval.

```r
#defining a modified activity data frame to work on to add missing values
modified<-activityDF
#identify which intervals contaian missing data
nasIntervals<-unique(modified[is.na(modified$steps),]$interval)
#for each missing value in a specific interval the steps are filled with the median for that interval 
for (naInterval in nasIntervals)
{
  modified[modified$interval == naInterval & is.na(modified$steps),]$steps<-median(modified[modified$interval == naInterval,]$steps, na.rm = TRUE)
}
numNasAfterFilling<-sum(is.na(modified$steps))
```

After the filling the number of missing values is 0.

The new data set without missing values:

```r
head(modified)
```

```
##   steps       date interval           date.time
## 1     0 2012-10-01        0 2012-10-01 00:00:00
## 2     0 2012-10-01        5 2012-10-01 00:05:00
## 3     0 2012-10-01       10 2012-10-01 00:10:00
## 4     0 2012-10-01       15 2012-10-01 00:15:00
## 5     0 2012-10-01       20 2012-10-01 00:20:00
## 6     0 2012-10-01       25 2012-10-01 00:25:00
```
Getting all the steps done per day after filling missing values

```r
totalAfterFilling<-tapply(modified$steps, modified$date, sum, na.rm = TRUE)
```
Histogram is made

```r
hist(totalAfterFilling, breaks = 10, main = "Histogram of steps per day after filling missing values", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)

Mean:

```r
mean(totalAfterFilling)
```

```
## [1] 9503.869
```
Median:

```r
median(totalAfterFilling)
```

```
## [1] 10395
```


####Do these values differ from the estimates from the first part of the assignment?  
The mean has a small difference, while the median is the same as before filling the missing values.  

####What is the impact of imputing missing data on the estimates of the total daily number of steps?  
The impact is quite limited given the policy chose of replacing the missing values with the median o steps in the corresponding intervals. Of course filling values has an impact on the mean of the daily steps which increases.



## Are there differences in activity patterns between weekdays and weekends?

Creating the factor variable to represent the weekday or weekend.

```r
#every day is first assigned as "weekday""
modified$typeDay<-"weekday"
#saturday and sunday are re-assigned as "weekend"
head(modified[weekdays(modified$date) == "Sunday"|weekdays(modified$date) == "Saturday",]$typeDay<-"weekend")
```

```
## [1] "weekend"
```

```r
#setting as factor
modified$typeDay<-as.factor(modified$typeDay)
```

Aggregation of the steps per interval and per type of day and computing the mean of steps done. 

```r
#aggregation and mean computation
totalWeekdayWeekend<-aggregate(modified$steps, by=list(modified$interval,modified$typeDay),mean)
names(totalWeekdayWeekend)<-c("interval","typeDay","steps")
```

Making a plot of the steps per day per type of day using ggplot:

```r
library(ggplot2)
  p<-qplot(x = interval, y = steps, data = totalWeekdayWeekend, facets = .~typeDay, xlab = "Interval", ylab = "number of steps", main = "Number of steps per type of day") + geom_line()
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)



