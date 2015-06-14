# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
#Load the data 
data <- read.csv("activity.csv")

#Process/transform the data (if necessary) into a format suitable for your analysis
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?


```r
# Calculate the total number of steps taken per day. Missing values can be ignored.
total_steps_per_day <- aggregate(x = data$steps , by = list(data$date), FUN = sum ,na.rm=TRUE)
colnames(total_steps_per_day) <- c("date","steps")
total_steps_per_day$steps <- as.numeric(total_steps_per_day$steps)

# Create a histogram of the total number of steps taken each day.
#png(filename = "figure/plot1.png", width = 480, height = 480)
hist(total_steps_per_day$steps, col = "green", main = paste("Total number of steps taken each day"), xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/totalnumberofsteps-1.png) 

```r
#dev.off()

# Calculate and report the mean of the total number of steps taken per day
mean(total_steps_per_day$steps)
```

```
## [1] 9354.23
```

```r
# Calculate and report the median of the total number of steps taken per day
median(total_steps_per_day$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern? 
