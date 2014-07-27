rankall <- function(outcome, num="best"){

  doDebug  = FALSE
  #Read the data
  mydata<- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  colNames <- names(mydata)
  nColDF = length(colNames) ## This many columns in data frame 
  findHos = "Hospital.Name"
  findState = "State"

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

  ### Check if num is correct. Should be "worst", "best" or a number b/w
  ### 1 and max number of hospitals in the state which have relevant data
  
  if( !(is.numeric(num) | num=="worst" || num=="best")){
    return("num has invalid value")
  }

  choppedbystates <- split(mydata, mydata$State)
  statenames = names(choppedbystates)

  finalHospital<- vector()

  for (mystate in 1:length(choppedbystates)){
    state = statenames[mystate]
    # check if its > the max number of hospitals 
    subsettedDF <- subset(mydata, State==state)
    if(doDebug) cat("Will marginalize over index ", indexpossibleConditions[conditionFound], "\n")
    vecWithVals <- suppressWarnings(as.numeric(subsettedDF[,indexpossibleConditions[conditionFound]]))
    vecWithNames <- subsettedDF[, 2] ## get the hospital names
    lengthAll <- nrow(subsettedDF)
    
    checkVec<- (!is.na(vecWithVals))
    validVec<- vecWithVals[checkVec]
    validNames<- vecWithNames[checkVec]
    if(doDebug){
      cat("Here is validVed ", validVec, "\n")
      cat("Here is validNames ", validNames, "\n")
    }

    if(is.numeric(num) &  length(validVec) < num){
      finalHospital <- c(finalHospital, "NA")
    }

    else {
      
      ## Outcome, state and num are OK. Now find the ranking of the hospitals
      ## Return a data frame with state and hospital name at that ranking
      sortedVec<- sort(validVec)
      sortedIndices<- order(validVec) ## a vector of indices which "sort" the validVec
      if(doDebug) cat("Here is the sortedVector ", sortedVec, "\n")
      newnames <- vector()
      
      for(i in 1:length(sortedIndices)){
        tobeadded <- validNames[sortedIndices[i]]
        newnames<- c(newnames, tobeadded)
      }
      if(doDebug) cat("Here are the names corr. to sortedVector ", newnames, "\n")
      
      ## write routine to "sort" the hospital names
      uq<- unique(sortedVec) ## is a vector with unique values in sortedVec
      finalNames<-vector()
      
      countEntries <- 1
      
      for(i in 1:length(uq)){
        
        tmpx<- subset(sortedVec, sortedVec==uq[i])
        if(length(tmpx)==1 ){
          finalNames<-c(finalNames, newnames[countEntries])
          countEntries<- countEntries+1
        }
        else{
          whichX<- which(sortedVec==uq[i]) ## are indices
          if(doDebug) cat("here ... ", whichX, "\n")
          verytmpnames <- sort(newnames[whichX])
          if(doDebug) cat("printing tmpnames ", verytmpnames, "\n")
          for(j in 1:length(verytmpnames))
            finalNames<-c(finalNames, verytmpnames[j])
          countEntries<- countEntries+length(verytmpnames)
        }
        
      }## len(uq)
      
      resultingIndex<- numeric()
      
      if(!is.numeric(num)){
        
        if(num=="best")   xx<- min(sortedVec)
        else if(num=="worst") xx<- max(sortedVec)
        
        if(doDebug) print( subset(sortedVec, sortedVec==xx))
        xxindex<- which(sortedVec==xx)
        resultingIndex<- xxindex[1]
        
      }## num!is.numeric
      
      else resultingIndex = num
      
      result<-finalNames[resultingIndex]
      finalHospital<- c(finalHospital, result)

    } ## if num>nhosp... else
    
    if(doDebug){
      ranking<- c(1:length(finalNames))
      DF<- data.frame(Hospital.Name=finalNames, Rate=sortedVec, Rank=ranking)
      head(DF)
    }
    
  } ## for mystate in 1:nstates 

  finalDF<- data.frame(hospital=finalHospital,  state=statenames)
  rownames(finalDF)<- statenames
  return (finalDF)

  #for (i in 1:length(finalHospital))    cat (statenames[i]," ", finalHospital[i], "\n")
  

} ## rankall

