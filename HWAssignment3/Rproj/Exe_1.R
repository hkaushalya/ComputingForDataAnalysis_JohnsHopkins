#HW Assignment # 3

#plot 30-day mortality rates

outcome<-read.csv('../data/outcome-of-care-measures.csv', colClasses = "character")
head(outcome)
class(outcome)
outcome[,11]<-as.numeric(outcome[,11])
outcome[,17]<-as.numeric(outcome[,17])
outcome[,23]<-as.numeric(outcome[,23])
x.ranges<-c(range(outcome[,11], na.rm=TRUE), range(outcome[,17], na.rm=TRUE), range(outcome[,23], na.rm =TRUE))
x.range<-range(x.ranges)
x.range  #common x-range for all plots

#medians 
median.oc11<-round(median(outcome[,11], na.rm=TRUE), 2)
median.oc17<-round(median(outcome[,17], na.rm=TRUE), 2)
median.oc23<-round(median(outcome[,23], na.rm=TRUE), 2)


#means 
mean.oc11<-round(mean(outcome[,11], na.rm=TRUE), 2)
mean.oc17<-round(mean(outcome[,17], na.rm=TRUE), 2)
mean.oc23<-round(mean(outcome[,23], na.rm=TRUE), 2)

title.main.oc11<-expression(paste('Heart Attack (',bar('X'),'=', mean.oc11,')'))
title.main.oc17<-expression(paste('Heart Failure (',bar('X'),'=', mean.oc17,')'))
title.main.oc23<-expression(paste('Pneumonia (',bar('X'),'=', mean.oc23,')'))

#quartz(title='hw#3')
par(mfrow=c(3,1))
hist(outcome[,11], xlab='30-day Death Rate', main=title.main.oc11 ,xlim=x.range, prob=TRUE)  
abline(v=median.oc11, col='red')
lines(density(outcome[,11], na.rm=TRUE))
hist(outcome[,17], xlab='30-day Death Rate', main=title.main.oc17, xlim=x.range, prob=TRUE)  
abline(v=median.oc17, col='red')
lines(density(outcome[,17], na.rm=TRUE))
hist(outcome[,23], xlab='30-day Death Rate', main=title.main.oc23, xlim=x.range, prob=TRUE)  
abline(v=median.oc23, col='red')
lines(density(outcome[,23], na.rm=TRUE))

#plotting death rate by state
table(outcome$State)
dfoc2<-as.data.frame(table(outcome$State))
states.w20.hospitals<-subset(dfoc2,dfoc2$Freq>20)  #select states with min of 20 hospitals
states.w20.vec<-states.w20.hospitals[,1]    #select factor (State) for selection in 'outcome' 
states.w20.vec
outcome2<-subset(outcome, outcome$State %in% states.w20.vec)
head(outcome2)

#box plot of death rates by states
death<-outcome2[,11]
state<-outcome2$State
boxplot(death ~ state, ylab='30-day Death Rate', main='Heart Attack Death Rate by State', las=2)
#Challenge: sort the states by median 30-day death rate by st
bymedian<-with(outcome2, reorder(state, death, median))

boxplot(death ~ bymedian,
        ylab = "30-day Death Rate",
        main = "Heart Attach Death Rate by State", varwidth = FALSE,
        col = "lightgray", las=2)

