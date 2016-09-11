##libraries
library(dplyr)
library(knitr)
library(ggplot2)

##loadData}
data <- read.csv(unz("activity.zip", "activity.csv"),colClasses = c("numeric","Date","numeric"))

##mean
dataNoMissingValues <- filter(data,steps != "NA")
totalSteps <- dataNoMissingValues %>% group_by(date) %>%
  summarize(total = sum(steps))
averageStepsPerDay <- mean(totalSteps$total)

## histogram for average number of steps
png(file = "plot1.png", width = 480, height = 480) 
hist(totalSteps$total, col = "red", main = "Total Steps per Day",
     xlab = "Total Steps", ylab = "Frequency")
dev.off() 

##The computed median is as below:
med <- median(totalSteps$total)


## What is the average daily activity pattern?

##To obtain this information, it is required to group the data by the interval identifier and get the average steps for each group of identifier. Consequently, a time series plot can then be constructed using this data to see the average pattern of steps in a day. 

aveStepsInterval <- dataNoMissingValues %>%
  group_by(interval) %>%
  summarize(mean = mean(steps))

png(file = "plot2.png", width = 480, height = 480) 
with(aveStepsInterval, plot(interval, mean, type = "l", xlab = "Interval Identifier", ylab = "Number of Steps", main = "Average Daily Steps"))
dev.off() 

## Imputing missing values

##For the purpose of this assignment, a simple strategy is used to impute the missing values. Missing values are imputed by inserting the average daily steps for each interval. For example, if the number of steps is missing for the 20th interval identifier, then this is replaced with the average value of `r aveStepsInterval[aveStepsInterval$interval == 20,2]`. The followings are the steps involved.
##Calculate the number of NA values
totalNA <- sum(is.na(data$steps))

##Now, create a new dataset with the NAs replaced:

##merge data with the average steps per interval dataset i.e. aveStepsInterval
imputedData <- merge(data,aveStepsInterval,by="interval")

##replace NA with the corresponding values from the mean variable
imputedData  <- transform(imputedData, steps = ifelse(is.na(steps), mean, steps))

##drop the mean variable
imputedData <- select(imputedData, -(mean))

##Next, make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
totalStepsDataImputed <- imputedData %>% group_by(date) %>%
  summarize(total = sum(steps))

png(file = "plot3.png", width = 480, height = 480) 
hist(totalStepsDataImputed$total, col = "blue", main = "Total Steps per Day",
     xlab = "Total Steps", ylab = "Frequency")
dev.off() 

##The median and mean are computed as below:
medianDataImputed <- median(totalStepsDataImputed$total)
medianDataImputed
meanDataImputed <- median(totalStepsDataImputed$total)
meanDataImputed

## Are there differences in activity patterns between weekdays and weekends?

##assign days of weekends
weekEnds <- c("Saturday","Sunday")

##add the factor variable day.type
imputedData <- mutate(imputedData, dayType = factor((weekdays(date) %in% weekEnds), levels=c(TRUE, FALSE), labels=c('weekend', 'weekday')))

dataForPanelPlot <- imputedData %>% 
  group_by(dayType,interval) %>%
  summarize(mean = mean(steps))

png(file = "plot4.png", width = 480, height = 480) 
ggplot(dataForPanelPlot,aes(x=interval,y = mean))+
  geom_line(aes(fill = dayType))+
  facet_grid(dayType~.) +
  labs(title = "Average Daily Steps For Weekdays and Weekends")+
  labs(x="Interval Identifier", y="Number of Steps")
dev.off() 