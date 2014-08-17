###### Initialization

## Load required libraries
stopifnot(require(dplyr))
stopifnot(require(ggplot2))

## Read in the data and massage it to the desired formats/factors
zipName <- "./activity.zip"
unzip(zipName)
fileName <- "./activity.csv"
act <- read.csv(fileName, 
                header = TRUE, 
                na.strings = "NA", 
                colClasses = c("integer", "character", "integer"))
act$date <- as.Date(act$date, "%Y-%m-%d")
act$date <- as.factor(act$date)

###### Total number of steps per day
## get the total per day and plot it as a histogram
act.no_na <- na.omit(act)
steps.total <- summarise(group_by(act.no_na,date), sum(steps))
names(steps.total)[2] <- "total"
qplot(steps.total[, 2], 
      data = steps.total, 
      geom="histogram", 
      xlab="Steps", 
      ylab="Count",
      main="Total Steps per Day")

mean(steps.total[, 2])

median(steps.total[, 2])

steps.interval <- summarise(group_by(act.no_na, interval), mean(steps))
names(steps.interval)[2] <- "total"
ggplot(steps.interval, aes(interval, total)) +
  geom_line() +
  ylab("total number of steps") +
  ggtitle("Steps per 5 Minute Interval")

si.max <- max(steps.interval[, 2])  # max number of steps in an interval
steps.interval[steps.interval$total == si.max, 1] # interval with that max

nrow(act) - nrow(act.no_na)

## set the steps that are NA equal to the mean of the steps in that interval
a <- filter(act, !is.na(act$steps)) # all without NA
b <- filter(act, is.na(act$steps))  # get only NA values
b[b$interval == steps.interval$interval, 1] <- steps.interval[, 2]
acta <- rbind(a, b)

###### Total number of steps per day
## get the total per day and plot it as a histogram
steps.totalb <- summarise(group_by(acta,date), sum(steps))
names(steps.totalb)[2] <- "total"

qplot(steps.totalb[, 2], 
      data = steps.totalb, 
      geom="histogram", 
      color="red",
      xlab="Steps", 
      ylab="Count",
      main="Total Steps per Day")

mean(steps.totalb[, 2])
median(steps.totalb[, 2])

nrow(steps.total)
sum(steps.total[, 2])
sum(steps.total[, 2])/nrow(steps.total)
nrow(steps.totalb)
sum(steps.totalb[, 2])
sum(steps.totalb[, 2])/nrow(steps.totalb)

acta$date <- as.Date(acta$date)
acta$weekdays <- weekdays(acta$date)
d <- filter(acta, acta$weekdays %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
d$weekend <- "Weekday"
e <- filter(acta, acta$weekdays %in% c("Saturday", "Sunday"))
e$weekend <- "Weekend"
acta <- rbind(d, e)
acta$weekend <- factor(acta$weekend)

steps.interval.weekend <- summarise(group_by(acta, interval, weekend), mean(steps))
names(steps.interval.weekend)[3] <- "total"
ggplot(steps.interval.weekend, aes(interval, total)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  ylab("total number of steps") +
  ggtitle("Steps per 5 Minute Interval")
