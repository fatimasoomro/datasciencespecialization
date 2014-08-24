
plot4 <- function(data, fullscc, debug){
  
  if(debug) cat("\n------------------   ------------------   ------------------   ------------------   ---------------- \n",
                "Question4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?",
                "\n------------------   ------------------   ------------------   ------------------   ---------------- \n \n", sep="")
  
  ## Subset the above to take only the SCCs for Coal.  
  ## Once we have the correct dataframe, the rest is a copy-paste of plot3 code
  
  #tComb <- grep("comb", as.character(fullscc$EI.Sector),  ignore.case=TRUE)
  tCoal <- grep("coal", as.character(fullscc$EI.Sector),  ignore.case=TRUE)
  ## all elements in tCoal are in tComb. So we assume that tCoal has elements relating to coal combustion
  
  goodSCC <- as.character(fullscc$SCC[tCoal])
  data<- subset(data, SCC %in% goodSCC)
  
  lev <- levels( as.factor(data$year))
  

  ## creates data frame with one row. Will later be column bound with the values of emissions we find

  mysum<- vector()
  nObs <- vector()
    
  # Loop over the years (lev object) and subset the dataframe and get the sum
  counter <- 1
  for (i in 1:length(lev)){ ## loop over years
    
    myyear <- as.numeric(lev[i])
    if(debug) cat("Looking at year ", myyear,  "\n", sep="")
    
    df<- subset(data,  year==myyear)
    nObs[counter] <- nrow(df)
    mysum[counter] <- sum(df$Emissions, na.rm=TRUE)
    
    if(debug) cat("Found sum ", i, " = ", mysum[counter]," using ", nObs[counter], " observations \n", sep="")
    counter <- counter+1
  }

    
  if(!debug) png("plot4.png") ## open png devide to draw the plot onto 
  plot(mysum, main="Emissions from coal combustion - United states", ylab=paste("Emissions (tons)", sep=""), xlab="Year", axes=FALSE, ylim=c(0, max(mysum)) , xlim=c ( 0.5, length(lev)+0.5) )
  axis(1, at = c(1:length(mysum)), c(as.numeric(lev)) )
  axis(2, at = as.integer( seq(0, max(mysum), length.out = 4)),  as.integer(seq(0, max(mysum), length.out = 4) ))
  box()

  if(!debug) dev.off()
  
}


  
