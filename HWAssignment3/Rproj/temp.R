#
rm(list=ls())
require(graphics)
par(mfrow=c(2,1))
data(mtcars)
boxplot(mtcars$mpg~ mtcars$cyl)
bymedian<-with(mtcars,reorder(cyl,mpg,median))
boxplot(mpg ~ bymedian, data = mtcars,
        xlab = "Cyl.", ylab = "MPG",
        main = "MPG by No. of Cyl.", varwidth = FALSE,
        col = "lightgray")