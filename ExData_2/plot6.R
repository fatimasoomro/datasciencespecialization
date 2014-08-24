plot6 <- function( data, debug, doHeavy){

  if(debug) cat ("\n------------------   ------------------   ------------------   ------------------   ---------------- \n",
                 " Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle \n",
                 " sources in Los Angeles County, California (fips == \"06037\"). \n",
                 " Which city has seen greater changes over time in motor vehicle emissions? \n",
                 "------------------   ------------------   ------------------   ------------------   ---------------- \n \n", sep="")
  
  LA <- subset(data, fips == "06037")
  btmr <- subset(data, fips=="24510") ## Baltimore city
  ## Subset the above to take only the SCCs for motor vehicles
  ## Once we have the correct dataframe, the rest is a copy-paste of plot3 code
  findSCC<- c("^22010", "^22300")
  
  if(doHeavy){
    ## get the indices of the dataset with SCC codes only for heavy vehicles
    findSCC[1] <- paste(findSCC[1],"7", sep="")
    findSCC[2] <- paste(findSCC[2],"7", sep="")
  }
  
  ## get the indices of the dataset which start with the SCCs for vehicles (Section 4.6, page 198 of the documentation)
  if(debug) cat("Will find SCCs starting with ",findSCC[1]," ", findSCC[2], "\n", sep="")
  myIndices <- grep(findSCC[1], as.character(LA$SCC))
  myIndices <- append(myIndices, grep(findSCC[2], as.character(LA$SCC)))
  ## Subset data and get only the rows with the required SCCs
  LA<- LA[myIndices,]
  if(debug) cat("The number of rows in final LA dataset ", nrow(LA), "\n", sep="")

  myIndices <- grep(findSCC[1], as.character(btmr$SCC))
  myIndices <- append(myIndices, grep(findSCC[2], as.character(btmr$SCC)))
  ## Subset data and get only the rows with the required SCCs
  btmr<- btmr[myIndices,]
  if(debug) cat("The number of rows in final Baltimore dataset ", nrow(btmr), "\n", sep="")

  ## creates data frame with two rows. First is the year and the second column repeats the city name for as many times as there are years 
  levLA <- levels( as.factor(LA$year))
  finaldfLA <- data.frame(year=levLA, City=rep("LA", length(levLA)) )
  lev <- levels( as.factor(btmr$year))
  finaldf <- data.frame(year=lev, City=rep("Baltimore", length(lev)))

  mysumbtmr<- vector()
  nObsbtmr <- vector()
  mysumLA<- vector()
  nObsLA <- vector()
  
  ## Loop over the years (lev object) and subset the dataframe and get the sum
  counter <- 1

  nLoop <- length(lev)
  nLoopLA <- length(levLA)
  prevVal <- 0
  
  for (i in 1:nLoop){ ## loop over years
    
    myyear <- as.numeric(lev[i])
    if(debug) cat("Looking at year ", myyear,  "\n", sep="")
    df<- subset(btmr,   year==myyear)
    nObsbtmr[counter] <- nrow(df)
    sumvalue <- sum(df$Emissions, na.rm=TRUE)
    mysumbtmr[counter] <- sumvalue 
    if(debug) cat("Baltimore: Found sum ", i, " = ", mysumbtmr[counter]," using ", nObsbtmr[counter], " observations \n", sep="")

    if(i==1)    mysumbtmr[counter] <- 0
    else{
      tmpsum <-  mysumbtmr[counter] 
      mysumbtmr[counter] <- 100.0*(tmpsum -prevVal)/prevVal
    }

    prevVal <- sumvalue
    counter <- counter+1
  }

  counter <- 1
  for (i in 1:nLoopLA){ ## loop over years
    
    myyear <- as.numeric(levLA[i])
    if(debug) cat("Looking at year ", myyear, "\n", sep="")
    dfLA<- subset(LA,   year==myyear)
    nObsLA[counter] <- nrow(dfLA)
    sumvalue <- sum(dfLA$Emissions, na.rm=TRUE)
    mysumLA[counter] <-sumvalue
    
    if(debug) cat("LA: Found sum ", i, " = ", mysumLA[counter]," using ", nObsLA[counter], " observations \n", sep="")

    if(i==1)     mysumLA[counter] <- 0
    else{
      tmpsum <-  mysumLA[counter] 
      mysumLA[counter] <- 100.0*(prevVal - tmpsum)/prevVal
    }
    prevVal <- sumvalue
    counter <- counter+1
  }
  
  finaldf  <-cbind(finaldf, Emissions=mysumbtmr)
  finaldfLA<-cbind(finaldfLA, Emissions=mysumLA)
  finaldf <- rbind(finaldf, finaldfLA)

  p <- ggplot(data=finaldf, aes(x=year, y=Emissions, group=City)) 
  p <- p + geom_line(aes(colour=City, shape=City))
  p <- p + labs( y="% difference in emissions wrt previous year", x="Year")
  p <- p + ggtitle("Emissions from motor vehicles")
  ## open png devide to draw the plot onto 
  if(!debug) png("plot6.png")
  print(p)
  if(!debug) dev.off()
  
  if(debug) return(finaldf)    
}
