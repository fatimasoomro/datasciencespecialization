
mergedatasets <- function(dirname, dodebug){

  if (dirname=="") dirname <- "UCI_HAR_Dataset"
  nobs<- 561
  allOK <- FALSE

  ## ---------              ----------             -------- ## 
  ## Read files: The activity and subject index are read in as vectors.
  readthisfile <- paste(dirname, "/train/y_train.txt", sep="")
  activityTrain <- scan(readthisfile)
  readthisfile <- paste(dirname, "/train/subject_train.txt", sep="")
  subjectTrain <- scan(readthisfile)
  readthisfile <- paste(dirname, "/test/y_test.txt", sep="")
  activityTest <- scan(readthisfile)
  readthisfile <- paste(dirname, "/test/subject_test.txt", sep="")
  subjectTest <- scan(readthisfile)
  trainingsampleN <- length(activityTrain)
  testingsampleN <- length(activityTest)

  ## Check if lengths of subject and activity lists are the same 
  if(trainingsampleN == length(subjectTrain) & testingsampleN == length(subjectTest) ) allOK <- TRUE
  if(!allOK) return("vectors were not of same length... check your files ")
  allOK<- FALSE
  ## ---------              ----------             -------- ##
  
  ## ---------              ----------             -------- ## 
  ## The actual features data are read in as tables. Each table should have nobs columns
  ## and nrow should equal the length of the vectors of activity and subject index.
  ## Check if nrow of the table (test) == length of subject and activity lists of test sample 

  readthisfile <- paste(dirname, "/test/X_test.txt", sep="")
  measTest <-  read.table(readthisfile)
  if(testingsampleN == nrow(measTest)) allOK <- TRUE
  if(!allOK) return ( " problem with the testing data table" )
  allOK<- FALSE

  ## Check if nrow of the table (train) == length of subject and activity lists of training sample 
  readthisfile <- paste(dirname, "/train/X_train.txt", sep="")
  measTrain <- read.table(readthisfile)
  if(trainingsampleN == nrow(measTrain)) allOK <- TRUE
  if(!allOK) return ( " problem with the testing data table" )
  allOK<- FALSE
  ## ---------              ----------             -------- ##

  
  ## ---------              ----------             -------- ##
  ## Select features which have mean and std in their name. Store their indices in a vector calles featIndices 
  features <- paste(dirname, "/features.txt", sep="")

  #Below, will return a list of two vectors, first is an integer and second is a string vector 
  feactureScan <- scan(features, what = list( num=0, feat=""))
  # Below get the feature names as a vector 
  listoffeat <- feactureScan$feat
  if (dodebug) cat(" length of list of features ", length(listoffeat), "\n" , sep="")
  
  # Below, loop over the features and find which of them are means or std. deviations, store the indices
  featIndices <- vector()
  j <- 0
  
  for (i in 1:length(listoffeat)){
    
    if ( grepl ("mean", listoffeat[i], ignore.case=TRUE) | grepl ("std", listoffeat[i], ignore.case=TRUE) )   {
      j <- j+1
      featIndices[j] <- i
    }
  }
  if(dodebug) cat("found ", j," indices \n", sep="")
  j<-1

  ## ---------              ----------             -------- ##


  ## ---------              ----------             -------- ##
  ## Append the testing and training samples, give meaningful names to the activity instead of the number.
  ## Then find the features which have mean and std in their name, change their name to not have -, ) or , in them
  ## Make a vector which has values both from the training and testing data
  ## cbind that vector to the dataframe we made above
  
  activityTrain<- append(activityTrain, activityTest, after = length(activityTrain)) # append a vector to a vector
  subjectTrain <- append(subjectTrain,  subjectTest,  after = length(subjectTrain)) # append a vector to a vector
  if(dodebug) cat("appended the vectors of activity and subject. Their lengths ", length(activityTrain), " ", length(subjectTrain), "\n", sep="")
  if ( length(activityTrain) == length(subjectTrain) ) allOK = TRUE
  if (!allOK) return ("appended the vectors of activity and subject but their lengths were not equal " )

  ## make a vector with the names of the activities and not the number 
  finalNumberOfRows<- length(activityTrain)
  actNames <- c("WALKING", "WALKING UPSTAIRS", "WALKING DOWNSTAIRS" , "SITTING", "STANDING", "LAYING")
  activities <- vector()
  for(i  in 1:finalNumberOfRows ) {
    activities[i] <- actNames[ activityTrain[i]]
  }
  ## create data frame with the activities (with names) and subjects
  df<- data.frame("Activity"=activities, "Subject"=subjectTrain)
  orignames<- names(df)
  origlen <- length(orignames)
 
  for(i in 1:length(featIndices)){
  # find a good column name
    colname <- ""
    startingStr <- listoffeat[featIndices[i]]
    
    tmpname<- unlist( strsplit(startingStr, "-"))
    for (j in 1:(length(tmpname)))      colname <- paste(colname, tmpname[j], sep="")
    if(dodebug) cat("1 ==>  column name  ", colname, "\n", sep="")
    
    colname<- chartr("(", "-" , colname)
    tmpname<- unlist( strsplit(colname, "-"))
    colname <- ""
    for (j in 1:(length(tmpname)))     colname <- paste(colname, tmpname[j], sep="")
    if(dodebug) cat("2 ==>  column name  ", colname, "\n", sep="")
    
    colname<- chartr(")", "-" , colname)
    tmpname<- unlist( strsplit(colname, "-"))
    colname <- ""
    for (j in 1:(length(tmpname)))     colname <- paste(colname, tmpname[j], sep="")
    if(dodebug) cat("3 ==>  column name  ", colname, "\n", sep="")

    colname<- chartr(",", "-" , colname)
    tmpname<- unlist( strsplit(colname, "-"))
    colname <- ""
    for (j in 1:(length(tmpname)))     colname <- paste(colname, tmpname[j], sep="")
    if(dodebug) cat("4 ==>  column name  ", colname, "\n", sep="")
    
    if(dodebug) cat(" column name to bind to df ", colname, "\n", sep="")
    orignames[origlen + i] <- colname
    
    # append the same column from the test and train data sets to tmpVec
    tmpVec<- measTest[,  featIndices[i]]
    tmpVec<- append(tmpVec,  measTrain[, featIndices[i]], after=length(tmpVec))
    # bind it to the data frame  
    df<-cbind(df, "name"=tmpVec)
  }
  ## ---------              ----------             -------- ##

  ## ---------              ----------             -------- ##
  ## Change the names of columns in the final data frame and return it
  if(dodebug) print(orignames)
  #orignames<- append(orignames, othernames, after=length(orignames))
  names(df) <- orignames
  return (df)
  ## ---------              ----------             -------- ##
  
} # function 


makeaverages<- function(origDF, dodebug=FALSE){

  ## Take as input a dataframe which has only the features with mean or std in their name,
  ## make another dataframe which has the averages of these features for each subject and
  ## each activity

  ## For debugging, used if dodebug is TRUE 
  maxCols <- 10 
  if(!dodebug)   maxCols <- ncol(origDF)
  ## ---------              ----------             -------- ##
  ## Make a vector of the unique activity and subject labels 
  y <- unique(origDF$Activity)
  y<- levels(y)
  if(dodebug) print(y)

  subs <- unique(origDF$Subject)
  if(dodebug) print(subs)
  ## ---------              ----------             -------- ##

  
  ## ---------              ----------             -------- ##
  ## Make a "skeleton" dataframe which has the same number of columns as the original one
  ## so that then for each subject and activity, we can rbind the averages of all features
  ## We will remove this first row at the end of the function before returning this dataframe
  dataframe<- data.frame("Activity"="tmp", "Subject"=-10, stringsAsFactors=FALSE)
  placeholder<- seq(1:(maxCols-2))
  for(i in 1:length(placeholder)) dataframe<- cbind(dataframe, placeholder[i])
  names(dataframe)<- names(origDF)[seq(1,maxCols)]
  ## ---------              ----------             -------- ##

  
  ## ---------              ----------             -------- ##
  ## Loop over all subject ids, and get a subsetted dataset for each. Inside this loop,
  ## loop over the activites and get further subsetted dataset i.e. a dataset for only
  ## one subject and one activity. Then loop over the feature columns, take each of them 
  ## as a vector and store its average in a vector.
  ## Then make a dataframe with activity and subject id and, in a loop, cbind the vector of
  ## averages to it. This dataframe should now be in the form we made above, so rbind it to
  ## the above made dataframe
  
  for(intSb in 1:length(subs)){ ## loop over subjects 
    df <- subset(origDF, origDF$Subject==subs[intSb])
    if(dodebug) cat("nrows in df ", nrow(df), "\n", sep="")
    
    for(intAc in 1: length(y)){ ## loop over activity 
      df2<- subset(df, df$Activity==y[intAc])
      if(dodebug)  cat("nrows in df2 ", nrow(df2), "\n", sep="")

      measurements<-vector()
      finalElements <- 0
      
      for(intMeas in 3:maxCols ){
        
        vec<- df2[,intMeas]
        finalElements <- finalElements+1
        measurements[finalElements] <- mean(vec) 
         if(dodebug)  cat("average is ",mean(vec), "\n", sep="")
      } # for intMeas
      if(dodebug) cat("stored ", finalElements, " measurement means in vector \n", sep="")
      dftmp<- data.frame("Activity"=as.character(y[intAc]), "Subject"=subs[intSb], stringsAsFactors=FALSE)
      for(i in 1:length(measurements)) dftmp<- cbind(dftmp, measurements[i])
      names(dftmp)<- names(origDF)[seq(1,maxCols)]
      dataframe<- rbind(dataframe, dftmp)
    } # for intSb 
  } # for intAc
  ## ---------              ----------             -------- ##
  
  ## remove the first row. We made this just to get a frame to rbind to 
  dataframe <- dataframe[seq(2,nrow(dataframe)),]
  write.csv(file="mysolutiondataframe.csv", dataframe, row.names = FALSE)
  
 }# function
