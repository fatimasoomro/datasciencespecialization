best <- function(state, outcome){

  doDebug  = FALSE
  #Read the data
  mydata<- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  colNames <- names(mydata)
  nColDF = length(colNames) ## This many columns in data frame 
  findHos = "Hospital.Name"
  findState = "State"

  ## Check that state and outcome are valid
  ## If not, return error using stop() function

  ### Check state 
  choppedbystates <- split(mydata, mydata$State)
  statenames = names(choppedbystates)
  if ( state %in% statenames){
    if(doDebug) print("Found state")
  }
  else{
    stop("invalid state")
  }

  ## check if outcome is correct
  conditionFound = -100
  possibleConditions<- vector()
  indexpossibleConditions<- vector()
  hasToContain  <- "Hospital.30.Day.Death..Mortality..Rates.from."
  
  for (i in 1:nColDF){

    s <- colNames[i]
    #if( length(grep("Hospital.30.Day.Death..Mortality..Rates.from.", s)) >0){
    if( substr(s, 1, 45) == hasToContain){
      y<-substr(s, 46, nchar(s))
      if(doDebug) cat ("Found a condition ", y, "\n")
      possibleConditions <- c( possibleConditions, y)
      indexpossibleConditions <- c( indexpossibleConditions, i)
    }## if grep 
  }

  if (doDebug) {
    print(nchar(possibleConditions))
    cat(" indices of conditions ", indexpossibleConditions , "\n")
    cat("length of possible condition vector ", length(possibleConditions), "\n")
  }
  
  ## loop over possible conditions and see if any matches the given condition
  for( i in 1:length(possibleConditions)){

    tmp<- possibleConditions[i]
    if(nchar(tmp) == nchar(outcome) && nchar(outcome>7)) {
      if(grep ( substr(tmp, 7, nchar(tmp)), outcome, ignore.case = TRUE)) {
        if(doDebug) print ("condition matches")
        conditionFound = i
      }## if grep 
    }
    else{
      if (doDebug) cat(nchar(tmp),"  vs ",   nchar(outcome), "\n")
    }
  } ## for 

  if(conditionFound <0) stop("invalid outcome")
  
  ## Return hospital name with the lowest 30 day death rate 

  subsettedDF <- subset(mydata, State==state)
  if(doDebug) cat("Will marginalize over index ", indexpossibleConditions[conditionFound], "\n")
  vecWithVals <- as.numeric(subsettedDF[,indexpossibleConditions[conditionFound]])
  vecWithNames <- subsettedDF[, 2] ## get the hospital names
 
  minVal<- min(vecWithVals, na.rm=TRUE) ## get min value
  
  checkVec<- vecWithVals==minVal ## returns a true/false/na vector
  
  goldenIndices<- which(checkVec==TRUE) ## returns at which index is the condition true

  finalVec <- vecWithNames[goldenIndices]
  sort(finalVec)
  
  print(finalVec[1])

}
