#Computing for Data Analysis 
#Programming Assignment 2: 
source('getmonitor.R')
source('complete.R')

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations

  df.complete<-complete('specdata')
  df.passthres<-subset(df.complete, nobs>=threshold)
  nrows<-nrow(df.passthres)
  msg<-paste('nrows = ', nrows, sep='')
  print(msg)
  rm(msg)
  cor.vec<-c(rep(0,nrows))
  if (nrows>=1) {
    for (i in 1:nrows) {
      id<-df.passthres[i,1]
      print('id=')
      print(id)
      df.i<-getmonitor(id,'specdata')
      df.i.complete<-subset(df.i, nitrate != NA & sulfate != NA)
      ok<-complete.cases(df.i$sulafe,df.i$nitrate)
      print(sum(ok))
      #if (nrow(df.i.complete)>=1) {
      if (sum(ok)>0) {
        cor.vec[i]<-cor(df.i$sulfate,df.i$nitrate,use="complete.obs")
        print(cor(df.i$sulfate,df.i$nitrate,use="complete.obs"))
      }
    }
  }
  #print(cor.vec)
  cor.vec
}