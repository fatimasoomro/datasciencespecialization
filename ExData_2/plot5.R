plot5 <- function(data, debug, doHeavy ){

  if(debug) cat("\n------------------   ------------------   ------------------   ------------------   ---------------- \n",
                " Question5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? \n",
                "------------------   ------------------   ------------------   ------------------   ---------------- \n \n", sep="")

  ## Subset the above to take only the SCCs for motor vehicles
  ## Once we have the correct dataframe, the rest is a copy-paste of plot3 code
  data <- subset(data, fips=="24510") ## Baltimore city
  findSCC<- c("^22010", "^22300")

  if(doHeavy){
    ## get the indices of the dataset with SCC codes only for heavy vehicles
    findSCC[1] <- paste(findSCC[1],"7", sep="")
    findSCC[2] <- paste(findSCC[2],"7", sep="")
  }

  ## get the indices of the dataset which start with the SCCs for vehicles (Section 4.6, page 198 of the documentation)
  if(debug) cat("Will find SCCs starting with ",findSCC[1]," ", findSCC[2], "\n", sep="")
  myIndices <- grep(findSCC[1], as.character(data$SCC))
  myIndices <- append(myIndices, grep(findSCC[2], as.character(data$SCC)))

  ## Subset data and get only the rows with the required SCCs
  data<- data[myIndices,]
  if(debug) cat("The number of rows in final dataset ", nrow(data), "\n", sep="")

  lev <- levels( as.factor(data$year))
  emissionsources<- unique(data$type) # this is a character type
  nreps <- length(emissionsources)
  a<-sapply(emissionsources, function (x) rep(x, length(lev)))

  mysum<- vector()
  nObs <- vector()
    
  # Loop over the years (lev object) and subset the dataframe and get the sum
  counter <- 1
  for(t in 1:nreps){ ## loop over types 
    for (i in 1:length(lev)){ ## loop over years
      
      myyear <- as.numeric(lev[i])
      mysource<- emissionsources[t]
      if(debug) cat("Looking at year ", myyear, " and type ", mysource, "\n", sep="")
        
      df<- subset(data, as.character(type) == mysource &  year==myyear)
      nObs[counter] <- nrow(df)
      mysum[counter] <- sum(df$Emissions, na.rm=TRUE)
      
      if(debug) cat("Found sum ", i, " = ", mysum[counter]," using ", nObs[counter], " observations \n", sep="")
      counter <- counter+1
      }
    }# nreps
  

  if(!debug) png("plot5.png") ## open png devide to draw the plot onto 
  mytitle<-"Emissions from motor vehicle sources in Baltimore city"
  plot(mysum, main=mytitle, ylab=paste("Emissions (tons)", sep=""), xlab="Year", axes=FALSE, ylim=c(0, max(mysum)) , xlim=c ( 0.5, length(lev)+0.5) )
  axis(1, at = c(1:length(mysum)), c(as.numeric(lev)) )
  axis(2, at = as.integer( seq(0, max(mysum), length.out = 4)),  as.integer(seq(0, max(mysum), length.out = 4) ))
  box()


  if(!debug) dev.off()
  
}
