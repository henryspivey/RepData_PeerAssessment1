Steps Analysis
--------------

### Getting the code

    download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip','data.zip')
    unzip('data.zip')
    activity_data <- read.csv('activity.csv')
    head(activity_data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Basic processing and summary stats

    activity_data$date <- as.Date(activity_data$date)
    mean(is.na(activity_data$steps))

    ## [1] 0.1311475

    summary(activity_data)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

### 1. What is mean total number of steps taken per day?

    steps_per_day_total <- with(activity_data, aggregate(steps, by=list(date), sum))

    names(steps_per_day_total) <- c("date", 'steps')
    head(steps_per_day_total)

    ##         date steps
    ## 1 2012-10-01    NA
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

    plot(steps_per_day_total$steps ~ steps_per_day_total$date, main="Total Steps/Day", type="h", lwd=5, xlab="Date", ylab="Step Count")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### 2. What is the average daily activity pattern?

    stepsInterval<-aggregate(steps~interval,data=activity_data,mean,na.rm=TRUE)
    plot(steps~interval,data=stepsInterval,type="l", main="Average Steps/Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)
Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    stepsInterval[which.max(stepsInterval$steps), ]

    ##     interval    steps
    ## 104      835 206.1698

### 3. Imputing missing values

    summary(steps_per_day_total)

    ##       date                steps      
    ##  Min.   :2012-10-01   Min.   :   41  
    ##  1st Qu.:2012-10-16   1st Qu.: 8841  
    ##  Median :2012-10-31   Median :10765  
    ##  Mean   :2012-10-31   Mean   :10766  
    ##  3rd Qu.:2012-11-15   3rd Qu.:13294  
    ##  Max.   :2012-11-30   Max.   :21194  
    ##                       NA's   :8

    NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    imputed_data <- replace(activity_data, TRUE, lapply(activity_data, NA2mean))
    head(imputed_data)

    ##     steps       date interval
    ## 1 37.3826 2012-10-01        0
    ## 2 37.3826 2012-10-01        5
    ## 3 37.3826 2012-10-01       10
    ## 4 37.3826 2012-10-01       15
    ## 5 37.3826 2012-10-01       20
    ## 6 37.3826 2012-10-01       25

    imputed_data_steps_per_day <- aggregate(imputed_data$steps, by=list(imputed_data$date), sum)
    names(imputed_data_steps_per_day) <- c('date', 'steps')
    head(imputed_data_steps_per_day)

    ##         date    steps
    ## 1 2012-10-01 10766.19
    ## 2 2012-10-02   126.00
    ## 3 2012-10-03 11352.00
    ## 4 2012-10-04 12116.00
    ## 5 2012-10-05 13294.00
    ## 6 2012-10-06 15420.00

    plot(imputed_data_steps_per_day$steps ~ imputed_data_steps_per_day$date, main="(Imputed) Total Steps/Day", type="h", lwd=5, xlab="Date", ylab="Step Count")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Summary statistics of imputed data

    summary(imputed_data_steps_per_day)

    ##       date                steps      
    ##  Min.   :2012-10-01   Min.   :   41  
    ##  1st Qu.:2012-10-16   1st Qu.: 9819  
    ##  Median :2012-10-31   Median :10766  
    ##  Mean   :2012-10-31   Mean   :10766  
    ##  3rd Qu.:2012-11-15   3rd Qu.:12811  
    ##  Max.   :2012-11-30   Max.   :21194

### 4. Are there differences in activity patterns between weekdays and weekends?

    library(lattice)
    weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    #Use `%in%` and `weekdays` to create a logical vector
    #convert to `factor` and specify the `levels/labels`
    imputed_data$week_day <- factor((weekdays(imputed_data$date) %in% weekdays1), 
                       levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
    aggregate_by_weekday <- aggregate(steps~interval+week_day, data=imputed_data, mean)
    xyplot(steps ~ interval | week_day, data=aggregate_by_weekday, type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)
