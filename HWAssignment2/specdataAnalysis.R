#HW 2
rm(list=ls())
options(error-reover)  #produce traceback browser on error

library(psych)
library(sm)

setwd("/Users/samantha/Documents/Personal/Samantha/LearningMaterials/OnlineCourses_Coursera/ComputingForDataAnalysis_JohnsHopkins/HWAssignment2")
list.files()

df.spec<-read.csv("specdata/001.csv")
print(df.spec)
print("Original dimensions of the dataset:")
print(str(df.spec))

#remove all the records with NAs
df.spec<-na.omit(df.spec)
print("Dimensions of the dataset after removing NAs:")
print(str(df.spec))
par(mfrow=c(2,3))
hist(df.spec$sulfate)
plot(df.spec$Date,df.spec$sulfate)
boxplot(df.spec$sulfate)

hist(df.spec$nitrate)
plot(df.spec$Date,df.spec$nitrate)
boxplot(df.spec$nitrate)

#rm(list=ls())
