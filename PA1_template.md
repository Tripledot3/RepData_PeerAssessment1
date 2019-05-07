---
title: "PA1_template"
author: "Tripledot3"
date: "May 7, 2019"
output: 
  html_document: 
    keep_md: yes
---
I am going to load the data into R. Open the 3 libraries.

```r
Act <- read.csv("activity.csv")
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.5.2
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.5.2
```

```
## 
## Attaching package: 'plyr'
```

```
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.5.2
```
Adding in the weekday element and formatting the date

```r
Act$day <- weekdays(as.Date(Act$date))
Act$date <- ymd(as.character(Act$date))
steps_by_interval <- aggregate(steps ~ interval, Act, mean)
steps_by_day <- aggregate(steps ~ date, Act, sum)
clean <- Act[!is.na(Act$steps),]
```

Now I want to sort data properly to get the total of steps walked for each day


```r
Act_T <- aggregate(Act$steps, by = list(Date = Act$date), FUN = sum)
```

Now I will make a histogram of the steps


```r
hist(Act_T$x, xlab = "Steps", main = "Total Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



Lastly, I want to provide the mean and the median of the total steps per day


```r
rmean <- mean(Act_T$x, na.rm = TRUE)
rmean 
```

```
## [1] 10766.19
```

```r
rmedian <- median(Act_T$x, na.rm = TRUE)
rmedian
```

```
## [1] 10765
```

Now I want to get the total of steps walked for each day


```r
Act_M <- aggregate(Act$steps, by = list(interval = Act$interval), FUN = mean,  na.rm = TRUE)
```

Now I will make a  time series plot of the steps


```r
plot(Act_M$x, xlab = "Interval", ylab = "Steps", main = "Total Steps per day", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
 
 Find the Interval with the MAx
 

```r
MaxSteps <- max(Act_M$x)
Act_M[Act_M$x==MaxSteps,1]
```

```
## [1] 835
```
Provide the count of NA's within the data


```r
nrow(Act[is.na(Act$steps),])
```

```
## [1] 2304
```
Missing data is being Imputed, replaced by the average

```r
incomplete <- sum(!complete.cases(Act))
imputed_data <- transform(Act, steps = ifelse(is.na(Act$steps), steps_by_interval$steps[match(Act$interval, steps_by_interval$interval)], Act$steps))
```
Imputed the first day into Zero

```r
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```
Creating Histogram

```r
steps_by_interval <- aggregate(steps ~ interval, Act, mean)
steps_by_day <- aggregate(steps ~ date, Act, sum)
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


Getting the Mean & Median for the imputed data

```r
mean_imputed <- mean(steps_by_day_i$steps)
mean_imputed
```

```
## [1] 10589.69
```

```r
median_imputed <- median(steps_by_day_i$steps)
median_imputed 
```

```
## [1] 10766.19
```



Getting the difference of the imputed & total difference

```r
mean_differ <- mean_imputed - rmean
mean_differ
```

```
## [1] -176.4949
```

```r
median_differ <- median_imputed - rmedian
median_differ
```

```
## [1] 1.188679
```

```r
total_differ <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)
total_differ
```

```
## [1] 75363.32
```
Creating the Plot

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.5.3
```

```r
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
