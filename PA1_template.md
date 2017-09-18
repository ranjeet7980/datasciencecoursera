Let's first read the data
=========================

    FootStep <- read.csv(file="C:/Users/Priyanka/Documents/Reproducible/activity.csv",header = T,sep = ",",stringsAsFactors = FALSE)

compute the date
================

    FootStep$date <-as.Date(FootStep$date,"%Y-%m-%d")
    str(FootStep)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

Calculate total number of steps taken each day
----------------------------------------------

    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.4.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

      MeanTotal <- FootStep %>% group_by(date) %>%
      summarize(TotalSteps = sum(steps,na.rm = T),
      MeanSteps = mean(steps,na.rm = T))

    ## Warning: package 'bindrcpp' was built under R version 3.4.1

Histogram of the total number of steps taken each day
-----------------------------------------------------

      with(MeanTotal, hist(TotalSteps, xlab = 'Total Steps per day', main = 'Frequency', col =))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)
conclusion: Graph is showing the hightest steps taken during the period
(10000 - 15000).

Calculate and report the mean and median of the total number of steps taken per day
===================================================================================

      summary(MeanTotal$TotalSteps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    6778   10395    9354   12811   21194

      summary(MeanTotal$MeanSteps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.1424 30.6979 37.3785 37.3826 46.1597 73.5903       8

What is the average daily activity pattern?
===========================================

      AverageDailyPattern <- FootStep %>% group_by(interval) %>%
        summarize(MeanSteps = mean(steps,na.rm = T))

1.make time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,averaged across all days (y-axis).
====================================================================================================================================================

      plot(AverageDailyPattern, xlab = 'interval', ylab = 'Average number of steps taken', type   = 'l')

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?
============================================================================================================

Ans: Maxium number of steps taken between the interval (500-1000).

Imputing missing values
=======================

1.Calculate and report the total number of missing values in the dataset(i.e the total number of rows with NAs)
===============================================================================================================

      mean(is.na(FootStep$steps))

    ## [1] 0.1311475

      sum(is.na(FootStep$steps))

    ## [1] 2304

Conclusion: Near about thirteen percent of the values are missing from
the data.

As we can see we don't have any missing values here now.
========================================================

      sum(is.na(AverageDailyPattern$MeanSteps))

    ## [1] 0

Create a new dataset (totalNA) that is equal to the original dataset but with the missing data filled in.
=========================================================================================================

      totalNA <- FootStep
      for (j in 1:nrow(totalNA)) {
        if (is.na(totalNA$steps[j])) {
          index <- totalNA$interval[j]
          value <- subset(AverageDailyPattern, interval==index)
          totalNA$steps[j] <- value$MeanSteps}
      }
      
      head(totalNA)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

      MeanTotal2 <- totalNA %>% group_by(date)%>%
        summarize (TotalSteps = sum(steps,na.rm=T),
      MeanSteps = mean(steps,na.rm = T))

Graph
=====

    with(MeanTotal2, hist(TotalSteps, xlab = 'Total Steps per day', main = 'Frequency', col = 'blue'))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)
Conclusion:Graph is showing the hightest steps taken during the period
(10000 - 15000) again with new datasetbut this time with a littel
increase in the frequency as compare to previous data.Yes there is
little difference in their mean and median.

Difference of two datasets
==========================

    summary(MeanTotal$TotalSteps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    6778   10395    9354   12811   21194

    summary(MeanTotal2$TotalSteps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

      totalNA$day<- ifelse(weekdays(totalNA$date) %in% c("Saturday", "Sunday"), "weekend",   "weekday")

      Weekend <- filter(totalNA, day == "weekend")
      Weekday <- filter(totalNA, day == "weekday")
      Weekend <- Weekend %>% group_by(interval) %>%
      summarize(MeanSteps = mean(steps)) 
      Weekend$day <- "weekend"

      Weekday <- Weekday %>% group_by(interval) %>%
      summarize(MeanSteps = mean(steps)) 
      Weekday$day <- "weekday"

      ActivityPattern <- rbind(Weekend, Weekday)
      ActivityPattern$day <- as.factor(ActivityPattern$day)
      ActivityPattern$day <- relevel(ActivityPattern$day, "weekend")

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
=====================================================================================================================================================================================================

      library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.4.1

      g <- ggplot(ActivityPattern, aes (interval, MeanSteps))
      g + geom_line() + facet_grid (day~.)+ labs(y = "Average number of steps taken") + labs(x   = "Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
Conclusion: Yes,there is lot of difference between activity patterns
between weekdays and weekends.As we can see from the graph that there is
no high level activity for weekend data and the activity is preity
constant through out the period of time but for weekday we can see high
level activity at particularpoint of time only.
