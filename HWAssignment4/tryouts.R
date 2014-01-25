#HW#4
#notes from video lecture

homicides<- readLines('homicides.txt')
homicides[1]
homicides[1000]
length(grep("iconHomicideShooting|icon_homicide_shooting", homicides))
length(grep("Cause: shooting", homicides))
length(grep("Cause: [Ss]hooting", homicides)) #yield 1003 records
length(grep("[Ss]hooting", homicides)) #yield 1005 records

i <- grep("Cause: [Ss]hooting", homicides) #yield 1003 records
j <- grep("[Ss]hooting", homicides) #yield 1005 records
str(i)
str(j)
setdiff(i,j) #returns '0'
setdiff(j,i) #returns '318' and '859'

homicides[859]

#grep has a option to return the value of the matching index (instead of the index)
