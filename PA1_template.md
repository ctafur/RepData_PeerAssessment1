# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

For this part of the assignment, first we ensure the *activity.csv* is within
the directory already unzipped. If not, we unzip it. Then we use the *read.csv*
function to load the data in an object called activity. We make sure to enforce
the option *stringsAsFactors=FALSE* in order to process the date column easily
later on. Finally, we change the class of the date column to *date*.


```r
# unzip activity file (if not already in directory)

if(!("activity.csv" %in% list.files())) {
        unzip("activity.zip")
}
        
# read the data

activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

# transform the date column to date format

activity$date <- as.Date(activity$date)
```



## What is mean total number of steps taken per day?

First, we calculate the total steps each day and create a new data frame. From
there we can extract the mean and median values.



```r
totals <- aggregate(steps ~ date, activity, sum)
total_mean <- mean(totals$steps)
total_median <- median(totals$steps)
```

This is a basic histogram of the total number of steps taken each day during two
months.


```r
with(totals, hist(steps))
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

In this dataset, the mean number of steps taken each day by this individual is
``10766`` and
``10765`` the median. 

## What is the average daily activity pattern?

To answer this, we take the *aggregate* function in basic R to compute
average steps every five minutes in these two months. Then we plot the result
in a basic line graph.


```r
# Create new data frame with the average steps every five minutes 

daily_by_interval <- aggregate(steps ~ interval, activity, mean)

daily_by_interval$time <- seq(as.POSIXct("2012-10-01 00:00:00 CET"),
                              as.POSIXct("2012-10-01 23:55:00 CET"), by="5 mins")
with(daily_by_interval, 
     plot(time, steps, type="l"))
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

The 5-minute interval with the average maximum value in this two months is the
one between

``835`` and ``840``.


## Imputing missing values

The summary function gives us in its report the number of missing NAs, which in 
the case of the steps column account for 2,304 missing values.


```r
summary(activity)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```
To remove NAs we are going to devise an strategy where each NA is replaced by
the average of the 5-minute interval where is that missing value.

To do that first we create a new data frame called *activity_full*, then we add
it the column *meanSteps*, where *meanSteps* is the average number of steps in
each 5-minute interval in the *daily_by_interval* object.
Then we replace each NA by its corresponding mean value.


```r
# create a copy of the data frame

activity_full <- activity

# add the mean steps in each 5-minute interval

activity_full$meanSteps <- daily_by_interval$steps

# replacement of values in new data frame

activity_full[is.na(activity_full$steps), 1] <- 
        activity_full[is.na(activity_full$steps), 4]
```

Finally, we are going to plot the original data frame and the one with no 
missing values alongside to spot the main differences.



```r
# plot options
par(mfrow=c(1,2))

# histogram of original dataset

with(totals, hist(steps, main="Original dataset histogram"))

# histogram of steps in new dataset

totals2 <- aggregate(steps ~ date, activity_full, sum)
with(totals2, hist(steps, main="Histogram of dataset with NAs replaced"))
```

![plot of chunk unnamed-chunk-8](./PA1_template_files/figure-html/unnamed-chunk-8.png) 

Due to the method we have employed to fill missing values, both histograms
look almost identical. The main difference is the one with the replaced values
has proportionally higher frequencies. If we take a look at the new mean and 
median values, we can check their values are very close to the original data.


```r
total2_mean <- mean(totals2$steps)
total2_median <- median(totals2$steps)
```
Mean = ``10766``

Median = ``10766``


## Are there differences in activity patterns between weekdays and weekends?

The graph below shows a more irregular step pattern on weekends, with
lots of highs and lows. The average steps between 10:00 and 20:00 are
higher on weekends as well, suggesting a more active lifestyle
at the end of the week.


```r
# add new column with weekdays

activity_full$days <- weekdays(activity_full[,2])

# add new column with only one level: weekday

activity_full$week <- "weekday"

# insert weekends where appropriate

activity_full[activity_full$days %in% c("Saturday", "Sunday"), 6] <- "weekend"

# convert to factor

activity_full$week <- as.factor(activity_full$week)

# compute average steps by interval and type of weekday

totals3 <- with(activity_full, aggregate(steps ~ interval + week, FUN=mean))

# plot results by day of the week

par(mfrow=c(2,1))
with(totals3[totals3$week=="weekday",],
     plot(interval, steps, type="l", main="Average steps on weekdays"))
with(totals3[totals3$week=="weekend",],
     plot(interval, steps, type="l", main="Average steps on weekends"))
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 

