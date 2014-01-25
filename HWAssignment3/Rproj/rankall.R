#rankall.R
#source('best.R')
source('rankhospital.R')

rankall<-function(outcome, num="best") {
  #Read outcome data
  df<-read.csv('../data/outcome-of-care-measures.csv', colClasses = "character")
  states.table<-table(df$State)
  states.table
  states<-names(states.table)
  states            #character vec of state abbr. names
  
  hospital <- c( rep('', length(states)) )
  ind <- 1
  for (st in states) {
    print(st)
    #print(ind)
    hospital[ind]<-rankhospital(st, outcome, num)
    ind <- ind + 1
  }
  df<-data.frame(cbind(hospital, states))
  names(df)<-c('hospital', 'state')
  return(df)
}

#state<-c('TX', 'VA','IL')
#trash<-lapply(state,best, outcome='heart attack')
#trash<-lapply(state,best, outcome='heart failure')
#trash<-lapply(state,best, outcome='pneumonia')
#head(rankall('heart attack', 20), 10)
#head(rankall('heart failure', 20), 10)