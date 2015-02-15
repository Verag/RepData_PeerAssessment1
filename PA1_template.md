# PA1_template.Rmd


## Loading and preprocessing the data

```
Unzip the file, load the data and coerce date variable to data format
```

```r
filename <- unzip("activity.zip")
RawData <- read.csv(filename, stringsAsFactors = FALSE)
str(RawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
RawData$date <- as.Date(RawData$date)
str(RawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```




## What is mean total number of steps taken per day?


```r
stepsdate <- aggregate(steps~date, data =RawData,FUN=sum)

hist(stepsdate$steps,main="Histogram of steps by date", xlab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(stepsdate$steps)
```

```
## [1] 10766.19
```

```r
median(stepsdate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
stepsinterval <- aggregate(steps~interval,data= RawData,FUN=mean)
plot(stepsinterval, type ="l", main="Steps by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
stepsinterval$interval[which.max(stepsinterval$steps)]
```

```
## [1] 835
```



## Imputing missing values

```r
sum(is.na(RawData))
```

```
## [1] 2304
```

```r
DatanotNA <- RawData
for (i in 1:nrow(RawData)) {
        if(is.na(RawData[i,"steps"])) {
                interval <- RawData[i,"interval"]
                index <- which(interval == stepsinterval[,"interval"])
                DatanotNA[i,"steps"] <- stepsinterval[index , "steps"]
        }
}
head(DatanotNA)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
stepsdatenotNA <- aggregate(steps~date, data =DatanotNA,FUN=sum)

hist(stepsdatenotNA$steps,main="Histogram of steps by date", xlab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(stepsdatenotNA$steps)
```

```
## [1] 10766.19
```

```r
median(stepsdatenotNA$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
DatanotNA$day_type <- weekdays(DatanotNA$date)
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
for(i in 1:nrow(DatanotNA)) {
        if(any(DatanotNA[i, "day_type"] == weekdays)) {
                DatanotNA[i, "day_type"] <- "Weekday"
        }
        else {
                DatanotNA[i, "day_type"] <- "Weekend"                
        }
}
DatanotNA$day_type <- factor(DatanotNA$day_type,levels = c("Weekday", "Weekend")) 
head(DatanotNA)
```

```
##       steps       date interval day_type
## 1 1.7169811 2012-10-01        0  Weekend
## 2 0.3396226 2012-10-01        5  Weekend
## 3 0.1320755 2012-10-01       10  Weekend
## 4 0.1509434 2012-10-01       15  Weekend
## 5 0.0754717 2012-10-01       20  Weekend
## 6 2.0943396 2012-10-01       25  Weekend
```

```r
stepsweekdays <- aggregate(steps ~ interval + day_type, DatanotNA, mean)
head(stepsweekdays)
```

```
##   interval day_type     steps
## 1        0  Weekend 1.7169811
## 2        5  Weekend 0.3396226
## 3       10  Weekend 0.1320755
## 4       15  Weekend 0.1509434
## 5       20  Weekend 0.0754717
## 6       25  Weekend 2.0943396
```

```r
library(lattice)
xyplot(steps ~ interval | day_type, stepsweekdays, layout = c(1, 2),
       col = "blue", xlab = "Interval", ylab = "Average number of steps", 
       type = "l", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
