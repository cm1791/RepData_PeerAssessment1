Introduction
------------

sudo tlmgr update --self sudo tlmgr install framed

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This project makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day. The
goal of this project is to write a report that answers the questions
detailed below.

Loading and preprocessing the data
----------------------------------

1.  Load the data from the 'activity.csv' into the R programme and look
    at the structure of the dataset.

<!-- -->

    library(ggplot2)
    knitr::opts_chunk$set(warning=FALSE)

    activity <- read.csv("activity.csv")
    activity[,2]<-as.Date(activity$date)
    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

1.  Look at the summary of the dataset.

<!-- -->

    summary(activity)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

### What is mean total number of steps taken per day?

First, we'll build the historgram of total steps taken per day.

    steps_1<-with(activity,tapply(steps,date,sum,na.rm=TRUE))
    hist(steps_1,col = "green",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

1.  Secondly, we will calculate and print the average total steps and
    median of total steps taken each day.

<!-- -->

    print(mean_steps<-mean(steps_1))

    ## [1] 9354.23

    print(median_steps<-median(steps_1))

    ## [1] 10395

The average total steps taken each day is 9354.2295082 and, the average
median is is calculated to be 10395. We can also look at the the summary
of the total steps taken each day.

    summary(steps_1)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    6778   10395    9354   12811   21194

### What is the average daily activity pattern?

1.  First, we will make a time series plot of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis).

<!-- -->

    avg_steps<-with(activity,tapply(steps,interval,mean,na.rm=TRUE))
    intervals<-unique(activity$interval)
    new<-data.frame(cbind(avg_steps,intervals))
    plot(new$intervals,new$avg_steps,type = "l",xlab = "Intervals",
         ylab = "Average Steps",main = "Average Steps per Interval") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    index<-which.max(new$avg_steps)
    max<-new[index,2]

The 5-minute interval that contains the maximum number of steps is 835.

### Imputing missing values

1.  We will, first, calculate the average of average steps per day
    across all dates in the data set (ignoring NA values). Then we will
    use the resulting value in place of NAs.

Here we create a new dataset that is equal to the original dataset but
with the missing data filled in.

    sum(is.na(activity$steps))

    ## [1] 2304

    index<-which(is.na(activity$steps))
    l<-length(index)
    steps_avg<-with(activity,tapply(steps,date,mean,na.rm=TRUE))
    na<-mean(steps_avg,na.rm = TRUE)
    for (i in 1:l) {
            activity[index[i],1]<-na
    }

1.  We will check if we filled all NAs properly and see how our new
    dataset looks like.

<!-- -->

    sum(is.na(activity$steps))

    ## [1] 0

We have zero NAs which means we have successfully filled all missing
data and new dataset looks like as follows.

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

Here we will create a new histogram of total steps taken each day with
the new dataset.

    steps_2<-with(activity,tapply(steps,date,sum,na.rm=TRUE))
    hist(steps_2,col = "green",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

1.  Calculate the mean and median total number of steps taken per day
    for the complete dataset.

<!-- -->

    print(mean_steps_2<-mean(steps_2))

    ## [1] 10766.19

    print(median_steps_2<-median(steps_2))

    ## [1] 10766.19

The average total steps taken each day is 1.076618910^{4} and the median
of total steps taken each day is 1.076618910^{4}. Notice that both the
average and the median of the total steps taken eah day after NAs are
filled came out to be equal.

### Are there differences in activity patterns between weekdays and weekends?

In this section, we will use dplyr package and we need to load it from
the library. And, we will need to create a new variable in the dataset
named “day” that shows the day of the week in terms of weekday or
weekend.

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
    activity$datetype <- sapply(activity$date, function(x) {
            if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                    {y <- "Weekend"} else 
                    {y <- "Weekday"}
                    y
            })
    activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
    plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
           geom_line() +
           labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
           facet_wrap(~datetype, ncol = 1, nrow=2)
    print(plot)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)
