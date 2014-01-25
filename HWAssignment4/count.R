#HW#4
CAUSES <- c('asphyxiation', 'blunt force', 'other', 'shooting', 'stabbing', 'unknown')
DEBUG <- F

count <- function(cause = NULL) { 

  ## Check that "cause" is non-NULL; else throw error 
  if ( is.null(cause))
  {
    stop('Cause cannot be NULL!')
  }
  ## Check that specific "cause" is allowed; else throw error
  if ( !(cause %in% CAUSES) )
  {
    stop('Undefined cause!')
  }
  ## Read "homicides.txt" data file 
  
  homicides <- readLines('homicides.txt')
  #homicides <- readLines('shooting.txt')
  
  ## Extract causes of death 
  search.words <- c("Cause:", as.character(cause))
  #search.string <- paste(search.words, sep='')
  search.string <- paste(search.words, collapse = ' ')
  if (DEBUG) {
    cause <- 'Cause: other'
    search.words <- cause
    searh.string <- 'Cause: other'
  }
  data <- 'NULL'
  #cat('lenght of search.string (', search.string ,') = ', length(search.string), '\n')
  if (cause == 'other') {
    data <- grep('Cause: other', homicides, ignore.case = TRUE, value = T)
  } else if (cause == 'asphyxiation') {
    data <- grep('Cause: asphyxiation', homicides, ignore.case = TRUE, value = T)    
  } else if (cause == 'blunt force') {
    data <- grep('Cause: blunt force', homicides, ignore.case = TRUE, value = T)    
  } else if (cause == 'stabbing') {
    data <- grep('Cause: stabbing', homicides, ignore.case = TRUE, value = T)    
  } else if (cause == 'unknown') {
    data <- grep('Cause: unknown', homicides, ignore.case = TRUE, value = T)    
  } else if (cause == 'shooting') {
    data <- grep('Cause: shooting', homicides, ignore.case = TRUE, value = T)    
 } else {
  
   stop('Undefined cause requested!')
  }
    
  
  #print(head(data))
  num.items <- length(data)
  cat('Number of items for', cause, ' = ' , num.items ,'\n')
  #cat(data, file='shooting_from_r.txt', sep = '\n')
  ## Return integer containing count of homicides for that cause
  rm(data, search.words, search.string, homicides)
  return (num.items)
}


#count ('other')
#count ('shooting')
print(sum(sapply(CAUSES, FUN=count)))
