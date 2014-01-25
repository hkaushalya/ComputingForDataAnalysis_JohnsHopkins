#Computing for Data Analysis 
#Programming Assignment 2: 
getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  
  #if ID is not 3 digits, must add 0's to the beginning
  
  #print(directory)
  #if ((as.logical(as.numeric(id[1])>300)) == TRUE) {
  #  print(id)
  #} else {
  #  print('we are good')
  #}
  filename<-sprintf("%s/%03d.csv", directory, as.numeric(id))
  print(filename)
  df<-data.frame()  #empty data frame
  X<-file.exists(filename)
  if (X[1] == FALSE) {
    msg<-paste('File named', filename, 'does not exist! returning NULL data frame!',sep=' ')
    print(msg)
    rm(msg)
    return(df)
  }
  df<-read.csv(filename, header=TRUE)

  if (summarize == TRUE) {
    print(summary(df))
  }
  
  df   #this is returned
}

#id<-1
#path<-c('/Users/samantha/Documents/Personal/Samantha/LearningMaterials/OnlineCourses_Coursera/ComputingForDataAnalysis_JohnsHopkins/HWAssignment2/specdata')
#path<-c('specdata')
#d<-getmonitor(id,path,FALSE)
#print(head(d))