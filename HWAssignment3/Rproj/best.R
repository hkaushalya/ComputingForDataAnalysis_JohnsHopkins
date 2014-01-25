#best.R

best<-function(state, outcome) {
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
  
  #isolate the data frame for the state based on the condition
  #need to find a better way for this (using apply or something)
  if (outcome == outcomes[1]) {
    #df2<-subset(df, State==state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    df2<-subset(df, State==state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    #now conver the rate column from character to numeric
    df2[,3]<-as.numeric(df2[,3])
    min.death.rate<-min(df2[,3], na.rm=TRUE)
    min.death.rate
    #isolate the records corresponding to the min.death.rate (can be more than one)
    df.mindeaths<-subset(df2,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min.death.rate)
    #need to sort the hospital by name
    #sort and just pick the first one irrespective of there is only one element
    # this will avoid a if/else
    hsp.names<-df.mindeaths$Hospital.Name
    hsp.names<-sort(hsp.names)
    print(hsp.names[1])    
  } else if (outcome == outcomes[2]) {
    df2<-subset(df, State==state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    #now conver the rate column from character to numeric
    df2[,3]<-as.numeric(df2[,3])
    min.death.rate<-min(df2[,3], na.rm=TRUE)
    min.death.rate
    #isolate the records corresponding to the min.death.rate (can be more than one)
    df.mindeaths<-subset(df2,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min.death.rate)
    #need to sort the hospital by name
    #sort and just pick the first one irrespective of there is only one element
    # this will avoid a if/else
    hsp.names<-df.mindeaths$Hospital.Name
    hsp.names<-sort(hsp.names)
    print(hsp.names[1])    
  } else if (outcome == outcomes[3]) {
    df2<-subset(df, State==state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    #now conver the rate column from character to numeric
    df2[,3]<-as.numeric(df2[,3])
    min.death.rate<-min(df2[,3], na.rm=TRUE)
    min.death.rate
    #isolate the records corresponding to the min.death.rate (can be more than one)
    df.mindeaths<-subset(df2,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min.death.rate)
    #need to sort the hospital by name
    #sort and just pick the first one irrespective of there is only one element
    # this will avoid a if/else
    hsp.names<-df.mindeaths$Hospital.Name
    hsp.names<-sort(hsp.names)
    print(hsp.names[1])    
  }
  
}

state<-c('TX', 'VA','IL')
best('TX','heart attack')
#trash<-lapply(state,best, outcome='heart attack')
#trash<-lapply(state,best, outcome='heart failure')
#trash<-lapply(state,best, outcome='pneumonia')

#best('TX',"heart attack")
#best('VA',"heart attack")
#best('IL',"heart attack")

#best("TT", "heart attack")
#best("TX", "heat attack")
