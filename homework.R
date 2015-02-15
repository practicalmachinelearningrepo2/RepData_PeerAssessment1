activity <- read.csv("C:/kaggle/reproducible data analysis/homework 1/RepData_PeerAssessment1/activity.csv")
activity[,2]<-as.Date(activity[,2])
activityClean<-activity[!is.na(activity$steps),]


stepsByDays<-aggregate(as.numeric(activityClean$steps), list(activityClean$date), FUN=sum)
hist(stepsByDays$x)
meanStepsPerDay<-mean(stepsByDays$x)
medianStepsPerDay<-median(stepsByDays$x)

stepsByIntervalsInDays<-aggregate(as.numeric(activityClean$steps), list(activityClean$interval), FUN=mean)
plot(stepsByIntervalsInDays,type="l")
stepsByIntervalsInDays[stepsByIntervalsInDays$x==max(stepsByIntervalsInDays$x),]

numberOfNAValues<-nrow(activity[is.na(activity$steps),])


colnames(stepsByIntervalsInDays)[1]<-"interval"
withMeanIntervalValues<-merge(activity,stepsByIntervalsInDays,by="interval")
withMeanIntervalValues$steps[is.na(withMeanIntervalValues$steps)] <- withMeanIntervalValues$x[is.na(withMeanIntervalValues$steps)]
withMeanIntervalValues<-withMeanIntervalValues[,1:3]

stepsByDaysImputed<-aggregate(as.numeric(withMeanIntervalValues$steps), list(withMeanIntervalValues$date), FUN=sum)
hist(stepsByDaysImputed$x)
meanStepsPerDayImputed<-mean(stepsByDaysImputed$x)
medianStepsPerDayImputed<-median(stepsByDaysImputed$x)

withMeanIntervalValues$isWeekday<-weekdays(withMeanIntervalValues$date)

withMeanIntervalValues$isWeekday[withMeanIntervalValues$isWeekday=='subota' | withMeanIntervalValues$isWeekday=='nedjelja']<-'weekend'
withMeanIntervalValues$isWeekday[withMeanIntervalValues$isWeekday!='weekend']<-'weekday'
withMeanIntervalValues$isWeekday<-as.factor(withMeanIntervalValues$isWeekday)

stepsByIntervalsInDaysImputed<-aggregate(as.numeric(withMeanIntervalValues$steps), list(withMeanIntervalValues$interval,withMeanIntervalValues$isWeekday), FUN=mean)

library(lattice)
xyplot(x~Group.1 | Group.2,data=stepsByIntervalsInDaysImputed,type='l',xlab='Interval',ylab='Number of steps',layout=c(1,2))
