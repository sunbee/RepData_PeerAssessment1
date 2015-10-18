# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Download the file using the specified link and unzip in the working directory.  


```r
setwd("C:/Users/ssbhat3/Desktop/Coursera ReproducibleResearch/RepData_PeerAssessment1/RepData_PeerAssessment1")
getwd()
Source <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
Destination <- "activity.zip"
download.file(Source, Destination, mode="wb") 
unzip(Destination)
```

This creates a file `activity.csv`. Read into a `data.table`.


```r
library(data.table)
DTact <- as.data.table(read.csv("activity.csv"))
DTact
```

```
##        steps       date interval
##     1:    NA 2012-10-01        0
##     2:    NA 2012-10-01        5
##     3:    NA 2012-10-01       10
##     4:    NA 2012-10-01       15
##     5:    NA 2012-10-01       20
##    ---                          
## 17564:    NA 2012-11-30     2335
## 17565:    NA 2012-11-30     2340
## 17566:    NA 2012-11-30     2345
## 17567:    NA 2012-11-30     2350
## 17568:    NA 2012-11-30     2355
```

Set the date in a format that `R` recognizes.


```r
DTact[, lapply(.SD, class)]
DTact[, date := as.Date(date)]
str(DTact)
```

We are now all set to process data.

## What is mean total number of steps taken per day?

Calculate the number of steps per day.

```r
DTact[, Total := sum(steps), by=date]
```

The histogram reveals the central tendency and spread.


```r
library(ggplot2)
DTact[, {
  m <- ggplot(.SD, aes(x=Total))
  m <- m + geom_histogram(aes(y=..density..), binwidth=3000, 
                          color="black", fill="wheat", alpha=0.2) 
  m <- m + geom_density(fill="gray", alpha=0.2)
  m <- m + geom_vline(aes(xintercept=mean(Total, na.rm=TRUE)),
                      color="red", linetype="dashed", size=1)
  m <- m + geom_vline(aes(xintercept=median(Total, na.rm=TRUE)),
                      color="blue", linetype="dashed", size=1)
  
}]
```

```
## Warning: Removed 2304 rows containing non-finite values (stat_density).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

The mean and median are shown on the histogram.


```r
DTact[, summary(Total, na.rm=TRUE)]
```

The total number of steps per day have `mean=10770` and `median=10760`. The overlay shows data are very noisy.

## What is the average daily activity pattern?

Let's look at any pattern in activity by time of day. The time of day is specified by interval number. We can average the number of steps in any interval across all days.


```r
DTint <- DTact[, .(interMean = mean(steps, na.rm=TRUE)), by=interval]
DTint[, {
  t <- ggplot(.SD, aes(interval, interMean))
  t <- t + geom_line()
  t <- t + geom_smooth()
}]
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The time-series reaffirm noisy data. 


```r
DTint[, Smoothed := {
  myLoess <- loess(data=.SD, interMean ~ interval)
  Smoothed <- myLoess$fitted
}]
DTint[, interval[c(which.max(Smoothed), which.max(interMean))]]
```

The maximum number of steps across all days is found in interval `835` in raw data. Given the data are noisy, smoothing may produce a more robust estimate. Applying loess method, the maxima is found in interval `900`.

## Imputing missing values

There are a number of records with missing values. 


```r
DTact[, sum(is.na(steps))]
```

```
## [1] 2304
```

There are `2304` records missing steps. Replace these with the median value in that interval. For this purpose, I have written a function `impute`. Then tweaked it for vectorization with `Vectorize`.


```r
impute <- function(x, y) {
  if (is.na(x)) {
    x <- y
  }
  x
}
vImpute <- Vectorize(impute)
```

The function replaces `NA` values with the median value in that interval.


```r
DTact[, interMedian := median(steps, na.rm=TRUE), by=interval]
DTact[, steps := vImpute(steps, interMedian)]
DTact[, interMedian := NULL]
```

Recompte totals with imputed values. Redo the histogram.


```r
DTact[, Total := sum(steps), by=date]
```

```
##        steps       date interval Total
##     1:     0 2012-10-01        0  1141
##     2:     0 2012-10-01        5  1141
##     3:     0 2012-10-01       10  1141
##     4:     0 2012-10-01       15  1141
##     5:     0 2012-10-01       20  1141
##    ---                                
## 17564:     0 2012-11-30     2335  1141
## 17565:     0 2012-11-30     2340  1141
## 17566:     0 2012-11-30     2345  1141
## 17567:     0 2012-11-30     2350  1141
## 17568:     0 2012-11-30     2355  1141
```

```r
DTact[, {
  m <- ggplot(.SD, aes(x=Total))
  m <- m + geom_histogram(aes(y=..density..), binwidth=3000, 
                          color="black", fill="wheat", alpha=0.2) 
  m <- m + geom_density(fill="gray", alpha=0.2)
  m <- m + geom_vline(aes(xintercept=mean(Total, na.rm=TRUE)),
                      color="red", linetype="dashed", size=1)
  m <- m + geom_vline(aes(xintercept=median(Total, na.rm=TRUE)),
                      color="blue", linetype="dashed", size=1)
  
}]
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
DTact[, summary(Total)]
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10400    9504   12810   21190
```

Imputation of values yields a mean of `9504` and median of `10400` steps per day. These adjusted values are lower than before.

## Are there differences in activity patterns between weekdays and weekends?

The function `wDay` determines whether a specified date is a weekday or a weekend. It's vectorized version is `vwDay`.


```r
wDay <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Saturday", "Sunday") ) {
    return("weekend")
  } else {
    return("weekday")
  }
}
wDay(as.Date("2015-10-18"))
```

```
## [1] "weekend"
```

```r
vwDay <- Vectorize(wDay)
```

Compute a new factor variable to indicate weekend. The time-series plot shows the activity on weekdays and weekends. 


```r
DTact[, day := factor(vwDay(date))]
```

```
##        steps       date interval Total     day
##     1:     0 2012-10-01        0  1141 weekday
##     2:     0 2012-10-01        5  1141 weekday
##     3:     0 2012-10-01       10  1141 weekday
##     4:     0 2012-10-01       15  1141 weekday
##     5:     0 2012-10-01       20  1141 weekday
##    ---                                        
## 17564:     0 2012-11-30     2335  1141 weekday
## 17565:     0 2012-11-30     2340  1141 weekday
## 17566:     0 2012-11-30     2345  1141 weekday
## 17567:     0 2012-11-30     2350  1141 weekday
## 17568:     0 2012-11-30     2355  1141 weekday
```

```r
DTwee <- DTact[, .(interMean = mean(steps, na.rm=TRUE)), 
      by=c("interval", "day")]
DTwee[, {
  t <- ggplot(.SD, aes(interval, interMean))
  t <- t + geom_line()
  t <- t + geom_smooth()
  t <- t + facet_grid(day ~ .)
}]
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

The plot shows difference in daily activity between weekday and weekend. We can fit a regression model to examine the assocation between day of the week and activity level. A regression model can show statistically significant differences from day to day.


```r
wDay <- DTact[, {
  lm(Total ~ day)         
}]
summary(wDay)
```

```
## 
## Call:
## lm(formula = Total ~ day)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9857.4 -2194.5   927.5  3479.6 12221.5 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8972.49      44.19  203.07   <2e-16 ***
## dayweekend   2025.89      86.27   23.48   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5030 on 17566 degrees of freedom
## Multiple R-squared:  0.03043,	Adjusted R-squared:  0.03038 
## F-statistic: 551.4 on 1 and 17566 DF,  p-value: < 2.2e-16
```

This  model shows that daily activity on a weekend is higher than the activity on a weekday and the difference is statistically significant at 95% confidence. The boxplot illustrates this result. 


```r
DTact[, lapply(.SD, class)]
```

```
##      steps date interval   Total    day
## 1: integer Date  integer integer factor
```

```r
DTact[, {
  p <- ggplot(.SD, aes(x=day, y=Total, fill=factor(day)))
  p <- p + geom_boxplot()
}] 
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 

## Conclusion

Data are noisy with over 10% records missing values. The imputation approach produces a preponderance of zero values. A more sophisticated approach may need to be considered. 

Regression shows that weekend affects the daily activity level and that the difference is statistically significant. It may be worthwhile looking closer at this association. A regression model with all 7 days of the week as the explanatory variables should be illuminatiing. 
