#Computing for Data Analysis 
#Programming Assignment 2: 

source('getmonitor.R')

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  #print(directory)
  #print(id)
  names<-c('id', 'nobs')
  cols<-factor(names)
  id.vec<-c(rep(0,length(id)))  #create null vector
  nobs.vec<-c(rep(0,length(id))) 
  
  #need this vector index to acount for cases like c(2,4,8) as ID
  index<-1  
  for (n in id) {
    msg<-sprintf('n=%d',n)
    #print(msg)
    id.vec[index]<-n
    #filename<-sprintf("%s/%03d.csv",directory,as.numeric(n))
    #print(str(filename))
    #df<-read.csv(filename,header=TRUE)
    df<-getmonitor(n,directory)
    #print(summary(df))
    #keep only complete cases
    #df[is.na(df)]<-0
    df2<-df[complete.cases(df),]
    #print(nrow(df2))
    nrow.df<-nrow(df2)
    nobs.vec[index]<-nrow.df
    #rm(msg, filename, df2, nrow.df)
    rm(msg, df2, nrow.df)
    index<-index+1
  }
  
  new.df<-data.frame(cbind(id.vec, nobs.vec))
  #print(head(new.df))
  print(new.df)
  colnames(new.df)<-names
  new.df
}

#all.df<-complete('specdata',c(2, 4, 8, 10, 12)) 
#print(all.df)
#all.df2<-complete("specdata", 30:25)
#print(all.df2)
#all.df3<-complete("specdata", 3)
#print(all.df3)
#all.df4<-complete("specdata", 1)
#print(all.df4)

