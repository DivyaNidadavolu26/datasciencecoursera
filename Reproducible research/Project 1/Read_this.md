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

Process the data into a suitable format to analyze

``` r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
is.regular(activity$date)
unique(activity$date)
```

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

[[https://github.com/DivyaNidadavolu26/datasciencecoursera/blob/master/Reproducible%20research/Project%201/Pictures/pic1.png]]

