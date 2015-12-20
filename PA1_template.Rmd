Reproducible Research - Peer Assessment 1
==================================================

### 1. Loading and processing the Data

```{r dataloading}
data<-read.csv("activity.csv")
```

### 2. What is mean total number of steps taken per day?

*   Calculate the total no. of steps taken per day

```{r meancal}
total_steps<-aggregate(steps ~ date, data, sum, na.rm=TRUE) #Creating a data frame of Sum of all the steps taken on a partcular date (ignoring the NA values)
```

*   Make a histogram of the total number of steps taken each day

```{r hist}
hist(total_steps$steps, main = "Total Steps Each Day", col="Maroon", xlab="Number of Steps")
```

*   Calculate and report the mean and median of the total number of steps taken per day
```{r mean&med}
rmean<-mean(total_steps$steps)
rmedian<-median(total_steps$steps)
```
The mean is `r rmean` and the median is `r rmedian`.


### 3. What is the average daily activity pattern?

*   Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Calculating the average:
```{r avgsteps}
steps_avg<-aggregate(steps ~ interval, data, mean) # The average of all steps w.r.t each interval is stored into the data frame steps_avg.
```
Plotting graph:
```{r plot}
plot(steps_avg, data$steps, type="l", col="Blue", main = "Average # of Steps per Day per Interval", xlab="5-Minute Interval")
```

*   Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r maxinterval}
max_interval<-steps_avg[which.max(steps_avg$steps),1] #Calculating maximum steps in data frame step_avg by using 'which' function.
```

On average across all the days, the 5-Minute interval which contains the maximum number of steps is `r max_interval`.


### 4. Imputing missing values

*   Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r nacount}
nacount<-sum(is.na(data$steps))
```

The total numbers of NAs are `r nacount`.

*   Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
imputed_data<-transform(data, steps = ifelse(is.na(data$steps), steps_avg$steps[match(data$interval,steps_avg$interval)],data$steps)) # Calculating the total data which is to be imputed

imputed_data[as.character(imputed_data$date) == "2012-10-01", 1]<-0
```

Every NA value in the dataset was replaced by zero.

*   Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
impsteps<-aggregate(steps ~ date, imputed_data, sum) # <-- This is the new dataset with the imputed values.
```

*   Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
hist(impsteps$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps") # Histogram of the imputed dataset.
# Calculating the new mean and median.
newmean<- mean(impsteps$steps)
newmedian<- median(impsteps$steps)
```
The new mean and meadian are `r newmean` and `r newmedian` respectively.

Calculating the difference between the new mean & median of the imputed data and the original mean & meadian.

```{r}
diff_mean<-newmean-rmean
diff_med<-newmedian-rmedian

#Calculating total difference
diff_total<-sum(impsteps$steps)-sum(steps_avg$steps)

```
*   The difference between the two means are `r diff_mean`.
*   The difference between the two meadians are `r diff_med`.
*   The difference between total number of steps between imputed and non-imputed data is `r diff_total` i.e. these many more steps in the imputed data.


### 4. Are there differences in activity patterns between weekdays and weekends?

*   Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
weekdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday") #storing the names of the days in an array Weekdays

imputed_data$dow=as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend")) #checking every date under ifelse statement. If the day is a weekday, it will store Weekday in the new variable dow.
```

*   Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
impsteps<-aggregate(steps ~ interval + dow,imputed_data,mean)

library(lattice) # To call function xyplot which is defined in library lattice.
xyplot(impsteps$steps ~ impsteps$interval|impsteps$dow, main="Average Steps / Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```














