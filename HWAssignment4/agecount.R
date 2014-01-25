#HW#4 part 2

agecount <- function(age = NULL) 
{ 
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age)) {
    stop('age cannot be null!')
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines('homicides.txt')

  age.as.char<- as.character(age)
  str <- paste(c(' ', age.as.char, ' years old'), collapse='')
  cat('search string = ', str, '\n')
  ## Extract ages of victims; ignore records where no age is ## given
  data <- grep(str, homicides, ignore.case = TRUE, value = T)
  
  num <- length(data)
  cat('num = ', num, '\n')
  rm (homicides, age.as.char, str)
  ## Return integer containing count of homicides for that age
  return (num)
}

#agecount(12)