Introduction
------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) </br> date: The date on which the measurement was taken in YYYY-MM-DD format </br> interval: Identifier for the 5-minute interval in which measurement was taken </br> The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
----------------------------------

Unzip and read the data from the repo.

``` r
library("data.table")
library(ggplot2)
activity <- read.csv(unz("activity.zip", "activity.csv"))
sapply(activity, class)
```

##     steps      date  interval 
## "integer"  "factor" "integer"


Process the data into a suitable format to analyze

``` r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
is.regular(activity$date)
unique(activity$date)
```

## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric

2. What is mean total number of steps taken per day?
----------------------------------------------------
2.1 Calculate the total number of steps taken per day

``` r
colMeans(is.na(activity))

data = activity

steps_per_day <- aggregate(steps ~ date, rm.na=TRUE,data,FUN=sum)
```

2.2. Make a histogram of the total number of steps taken each day

``` r
histo_show <- plot(steps_per_day, type="h",lwd=10,lend="square")

histo_show
```

![pic1](https://user-images.githubusercontent.com/34182120/45228173-16383180-b2e0-11e8-9990-b75f99940137.png)

2.3. Calculate and report the mean and median of the total number of steps taken per day.

``` r
step_date_function <- steps ~ date

data=activity

show_mean <- aggregate(step_date_function, data, FUN = mean)

show_mean

show_median <- aggregate(step_date_function, data, FUN =median)

show_median
```

What is the average daily activity pattern?
-------------------------------------------
3.1. Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
step_interval_function <- steps ~ interval

data = activity

show_graph <- plot(aggregate(step_interval_function, data, FUN=mean), type="l")

```

![pic2](https://user-images.githubusercontent.com/34182120/45228515-ff460f00-b2e0-11e8-85fa-e7bd1458af3d.png)

3.2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
max_value <- max(activity$steps, na.rm = TRUE)

max_value

```

## [1] 806

Imputing missing values
-----------------------
4.1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
na_sum_value <- sum(is.na(activity))

na_sum_value
```

## [1] 2304

4.2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
## Devise a strategy for filling in all of the missing values in the dataset.
## The strategy does not need to be sphisticated.
## For ex, you could use the mean/median for that day, or the mean for that 5-minute interval etc.

## The solution that I am thinking of is to substitute the value of NA with any fixed value.


## Then the fixed values are set to the equivalent value of the overall mean

## The overall mean shoulf be calculated using activity$steps
```

4.3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
second_activity <- activity

sapply(second_activity, class)


sec_act_steps <- second_activity$steps

sec_act_steps[is.na(sec_act_steps)] <- mean(na.omit(sec_act_steps))

sec_act_date <- second_activity$date
```

##     steps      date  interval 
## "integer"    "Date" "integer"

4.4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
data = second_activity

step_day2 <- aggregate(step_date_function, rm.na = TRUE, data , FUN = sum )

par(mfrow=c(1,2))

plot(steps_per_day, type="h",lwd=5,lend="square", main="With NAs")

abline(h=seq(0,20000,2500), lty="dashed")

plot(step_day2,type="h",lwd=5,lend="square",main="With NAs filled")

abline(h=seq(0,20000,2500),lty="dashed")

dev.off()
```
The following operations would be hiding interesting patterns such as the inactivity during particular days of week.

```{r}
fun1 <- aggregate(step_date_function, data = activity, FUN=mean)
fun1_1 <- aggregate(step_date_function, data = activity, FUN=median)

fun2 <- aggregate(step_date_function, data=second_activity, FUN=mean)
fun2_2 <- aggregate(step_date_function,data=second_activity, FUN=median)

```

![pic3](https://user-images.githubusercontent.com/34182120/45229412-5b119780-b2e3-11e8-8e4b-8107d643b51e.png)

## Are there differences in activity patterns between weekdays and weekends?

5.1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
second_activity$weekday <- factor(format(sec_act_date,"%A"))

levels(second_activity$weekday) <- list(weekday = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

```

5.2. Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
par(mfrow=c(2,1))

sec_act_weekday <- second_activity$weekday

func01 <- aggregate(steps_per_day ~ interval, FUN = mean)

with(second_activity[sec_act_weekday=="weekend",], plot(func01), main = "Weekends")

with(second_activity[sec_act_weekday=="weekday",], plot(func01), main="weekdays")

dev.off()
```

![pic4](https://user-images.githubusercontent.com/34182120/45229784-7fba3f00-b2e4-11e8-8066-2f23fcd5577f.png)
