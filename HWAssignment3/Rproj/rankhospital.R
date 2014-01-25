#rankhospital.R
DEBUG<-0
#FUNCTIONS<-c('')
#PrintFunction()
PrintLine<-function() {
  dots<-c(rep('==**',20))
  print(paste(dots,collapse=''))
}

CheckForMoreThanOne<-function(df, rate) {
  # Once a hospital is found with given RANK
  # this will look for other hospitals with same RANK.
  # If more than one hospital has the sam RANK, sort alphabetically
  # the hospital names and return the only the first.
  # 
  # Args:
  #   df = a data frame with 3 columns (Hospital.Name, State, 30-day.death.rate)
  #         death.rate can be any one of the 3 outcomes
  #   death.rate = death rate for the present outcome.
  #
  # Returns:
  #   First HospitalName in the list with the given death.rate
  #PrintLine()
  df.temp<-df[df[,3]==rate,]
  #print(df.temp)
  #print(rate)
  #print(nrow(df.temp))
  if (nrow(df.temp)>1) {
    #if (DEBUG==1) {
      #print(df.temp)
    #}
    df.final<-df.temp[order(df.temp[,1], decreasing=FALSE),]
    #print(df.final)
    return(df.final[1,1])
  } else {
    return(df.temp[1,1])
  }
  
}

server<-function(df2, num){
  
  #check if character args are given
  RANK<-NA  #default. will be used to ID 'worst' if num ==worst
  if (is.character(num)) {
    if (num=='best' || num == 'worst') {
      RANK<-1
    }
  } else {
    RANK<-num  
  }
  
  if (DEBUG) {
    cat(paste('Need rank =', RANK, sep=''))
  }
  
  #check if the rank requested is more than the number of hospital in that state and throw error if needed
  number.of.hospitals<-length(df2$Hospital.Name)
  if (RANK>number.of.hospitals) {
    return ('NA')
  }
  
  #now coerce the rate column from character to numeric
  df2[,3]<-as.numeric(df2[,3])
  #remove na
  df3<-na.omit(df2)
  #sort by death rate
  df4<-df3[order(df3[,3]),]
  
  if (nrow(df4)<RANK) {
    #stop('No Hospital with that rank!')
    #print(geterrmessage())
    return('NA')
  }
  if (DEBUG==1) { 
    print(head(df4))
  }
  
  if (num=='worst') {
    df4.worst<-tail(df4,n=RANK)
    return(CheckForMoreThanOne(df4, df4.worst[1,3]))
    #return(df4.worst$Hospital.Name)
  } else {
    return(CheckForMoreThanOne(df4, df4[RANK,3]))
    #return(df4$Hospital.Name[RANK])
  }
  
}

rankhospital<-function(state, outcome, num="best") {
  #Read outcome data
  df<-read.csv('../data/outcome-of-care-measures.csv', colClasses = "character")
  states.table<-table(df$State)
  states.table
  states<-names(states.table)
  states            #character vec of state abbr. names
  
  outcomes<-c('heart attack', 'heart failure', 'pneumonia')
  
  #check the state and outcome are valid  
  if ( ! (as.character(state) %in% states) )
  {
    #fix the quotes
    msg<-sprintf('Error in best (\"%s\", \"%s\") : invalid state', state, outcome)
    #cat(msg)
    #rm(msg)
    #return
    stop(msg)
  }
  
  if (! (as.character(outcome) %in% outcomes) ) {
    msg<-sprintf('Error in best ("%s", \"%s\") : invalid outcome', state, outcome)
    #cat(msg)
    #rm(msg)
    stop(msg)
  }
  
  #check if character args are given
  #RANK<-NA  #default. will be used to ID 'worst' if num ==worst
  #if (is.character(num)) {
  #  if (num=='best' || num == 'worst') {
  #    RANK<-1
  #  }
  #} else {
  #  RANK<-num  
  #}
  
  #if (DEBUG) {
  #  cat(paste('Need rank =', RANK, sep=''))
  #}

  #isolate the data frame for the state based on the condition
  #need to find a better way for this (using apply or something)
  if (outcome == outcomes[1]) {
    df2<-subset(df, State==state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    return(server(df2,num))
    
    #check if the rank requested is more than the number of hospital in that state and throw error if needed
    #number.of.hospitals<-length(df2$Hospital.Name)
    #if (RANK>number.of.hospitals) {
    #  return ('NA')
    #}
    
    #now coerce the rate column from character to numeric
    #df2[,3]<-as.numeric(df2[,3])
    #remove na
    #df3<-na.omit(df2)
    #sort by death rate
    #df4<-df3[order(df3[,3]),]
    
    #if (nrow(df4)<RANK) {
    #  stop('No Hospital with that rank!')
    #  print(geterrmessage())
    #}

    #if (num=='worst') {
    #  if (DEBUG) { 
    #    print(tail(df4,n=RANK))
    #  }
    #  df4.worst<-tail(df4,n=RANK)
    #  return(df4.worst$Hospital.Name)
    #} else {
    #  return(df4$Hospital.Name[RANK])
    #}
    
    
  } else if (outcome == outcomes[2]) {
    df2<-subset(df, State==state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    return(server(df2,num))
    
  } else if (outcome == outcomes[3]) {
    df2<-subset(df, State==state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    return(server(df2,num))
  }
  
}

#state<-c('TX', 'VA','IL')
#trash<-lapply(state,best, outcome='heart attack')
#trash<-lapply(state,best, outcome='heart failure')
#trash<-lapply(state,best, outcome='pneumonia')

#rankhospital('TX',"heart attack",4)
#rankhospital('TX',"heart attack",'best')
#rankhospital('TX',"heart attack",'worst')
#print(rankhospital('TX',"heart failure",4))
#print(rankhospital("MD", "heart attack", "worst"))
#print(rankhospital("MN", "heart attack", 5000))

#rankhospital('TX',"heart attack",16)

#best('VA',"heart attack")
#best('IL',"heart attack")

#best("TT", "heart attack")
#best("TX", "heat attack")
