plot3 <- function(data, debug){

  if(debug) cat("\n------------------   ------------------   ------------------   ------------------   ----------------  \n",
                " Question3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of \n",
                " these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases \n",
                " in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.\n",
                "------------------   ------------------   ------------------   ------------------   ----------------  \n \n", sep="")

  data <- subset(data, fips=="24510") ## Baltimore city
  
  lev <- levels( as.factor(data$year))
  emissionsources<- unique(data$type) # this is a character type
  nreps <- length(emissionsources)
  if(debug) cat("nreps ", nreps, "\n", sep="")
  a<-sapply(emissionsources, function (x) rep(x, length(lev)))
  if(debug) print(a)
  
  finaldf <- data.frame(year=rep(lev, nreps), type=as.vector(a))
  ## creates data frame with two rows. First is a repitition of the year in as many blocks as there are types.
  ## The second column repeats the type for as many times as there are years 
  ## For this case, is 4x4 so nrow will be 16
  
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
  
  finaldf<-cbind(finaldf, Emissions=mysum)

  # also works
  #qplot(year, Emissions, data=finaldf, color=type)
  # does not work 
  #ggplot(finaldf, aes(x=year, y=Emissions, colour=type)) + geom_point() + geom_smooth(se=FALSE, method="lm", aes(group=1))

  p <- ggplot(data=finaldf, aes(x=year, y=Emissions, group=type)) 
  p <- p + geom_line(aes(colour=type, shape=type))
  p <- p + labs( y="Emissions (tons)", x="Year")
  p <- p + ggtitle("Emissions for Baltimore city from different sources")
  if(!debug) png("plot3.png") ## Open graphics device
  print(p)
  if(!debug) dev.off()
 
}
