---
title: "Reproducible Research: Peer Assessment 1"
author: "Laica"
date: "June 2, 2019"
output: 
  html_document: 
    keep_md: yes
---

library(knitr)

## Loading and Pre-processing the data
## **Loading the data**

```r
activity <- read.csv("C:/Users/10012186/Documents/work/repdata_data_activity/activity.csv")
```
## **Loading the packages**

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
library(plyr)
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
library(ggplot2)
library(lattice)
```
## **Processing the data, creating new columns**

```r
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime <- as.POSIXct(activity$date, format="%Y-%m-%d")
```
#Cleaning the data
## **Pulling the data without NAs**

```r
clean_data <- activity[!is.na(activity$steps),]
```

# *What is the mean total of steps taken per day?*
## **Calculate the total steps taken**

```r
total_steps <- aggregate(activity$steps~activity$date, FUN=sum)
colnames(total_steps) <- c("Date", "Number of Steps")
```

## **Create a histogram of the total steps per day**

```r
hist(total_steps$`Number of Steps`, breaks = 5, xlab="Total Steps", main = "Total Count of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

##Mean of steps

```r
as.integer(mean(total_steps$`Number of Steps`))
```

```
## [1] 10766
```

##Median of Steps

```r
as.integer(median(total_steps$`Number of Steps`))
```

```
## [1] 10765
```
###The average number of steps taken per day was 10766
###The median number of steps taken per day was 10765

# *What is the average daily activity pattern?*

```r
steps_per_interval <- ddply(clean_data, .(interval), summarize, Avg=mean(steps))
```
## **Create time series plot of average number of steps per interval**

```r
steps_per_5min <- ggplot(steps_per_interval, aes(x=interval, y=Avg), xlab="Interval", ylab="Average Number of Steps")
steps_per_5min + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

#Which 5-minute Interval, on average across all the days in the dataset, contains the maximum number of steps? 
##Maximum steps by interval

```r
max_steps <- max(steps_per_interval$Avg)
```
##Which interval contains the maximum average number of steps?

```r
steps_per_interval[steps_per_interval$Avg==max_steps,1]
```

```
## [1] 835
```

###The maximum number of steps for each 5 minute interval is 206 steps 
###The interval that contains the maximum number of steps was the 835th interval

# *Imputing the missing values*
## **Calculate and report the total number of missing values in the dataset**
###Number of total NAs in the original data set

```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```
###The total number of rows with NA steps is 2304

## **Replace missing values** 
###Create the average number of steps per weekday and interval

```r
new_avg_steps <- ddply(clean_data, .(interval, day), summarize, Avg=mean(steps))
```

###Create dataset with just NAs

```r
NA_data <- activity[is.na(activity$steps),]
```

###Merge NA_data with new_avg_steps

```r
merged_data <- merge(NA_data, new_avg_steps, by=c("interval", "day"))
```

###Re-order the new table in the same format as the clean dataset

```r
merged_data2 <- merged_data[,c(6,4,1,2,5)]
colnames(merged_data2) <- c("steps", "date", "interval", "day", "DateTime")
```

###Merge the NA data and the non-NA data together

```r
new_merge_data <- rbind(clean_data, merged_data2)
```

##Create sum of steps per date to compare with step 1

```r
total_steps2 <- aggregate(new_merge_data$steps~new_merge_data$date, FUN=sum, )
colnames(total_steps2)<- c("Date", "Steps")
```

## Mean of Steps with NA data taken care of

```r
as.integer(mean(total_steps2$Steps))
```

```
## [1] 10821
```
## Median of Steps with NA data taken care of

```r
as.integer(median(total_steps2$Steps))
```

```
## [1] 11015
```

## **Creating the histogram of total steps per day, categorized by data set to show impact**

```r
hist(total_steps2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(total_steps$`Number of Steps`, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
###The new mean of the imputed data is 10821 steps compared to the old mean of 10766 steps. 
###This creates a difference of 55 steps on average per day.

###The new median of the imputed data is 11015 steps compared to the old median of 10765 steps. 
###That creates a difference of 250 steps for the median.

##However, the overall shape of the distribution has not changed.
## **Create new category based on the days of the week**

```r
new_merge_data$DayCategory <- ifelse(new_merge_data$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

### Summarize data by interval and type of day

```r
intervalTable2 <- ddply(new_merge_data, .(interval, DayCategory), summarize, Avg = mean(steps))
```

### Plot data in a panel plot

```r
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-25-1.png)<!-- -->
### The step activity trends are different on what day it occured.
