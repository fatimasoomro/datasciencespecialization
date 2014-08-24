
plot2 <- function(data, debug){

  
  if(debug) cat("\n------------------   ------------------   ------------------   ------------------   ----------------  \n",
                " Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == \"24510\") \n",
                " from 1999 to 2008? Use the base plotting system to make a plot answering this question. \n",
                "------------------   ------------------   ------------------   ------------------   ---------------- \n \n", sep="")


  data <- subset(data, fips=="24510" )
  lev <- levels( as.factor(data$year))

  mysum<- vector()
  nObs <- vector()
  
  # Loop over the years (lev object) and subset the dataframe and get the sum
  for (i in 1:length(lev)){
    
    myyear <- as.numeric(lev[i])
    if(debug) cat("Looking at year ", myyear, "\n", sep="")
    
    df<- subset(data,  year==myyear)
    nObs[i] <- nrow(df)
    mysum[i] <- sum(df$Emissions, na.rm=TRUE)
    
    if(debug)  cat("Found sum ", i, " = ", mysum[i]," using ", nObs[i], " observations \n", sep="")
  }

  if(!debug) png("plot2.png") ## open graphics device
  plot(mysum, main ="Total emissions from PM2.5 in Baltimore city", ylab=paste("Emissions (tons)", sep=""), xlab="Year", axes=FALSE, ylim=c(0, max(mysum)) , xlim=c ( 0.5, length(lev)+0.5) )
  axis(1, at = c(1:length(mysum)), c(as.numeric(lev)) )
  axis(2, at = as.integer( seq(0, max(mysum), length.out = 4)),  as.integer(seq(0, max(mysum), length.out = 4) ))
  box()
  if(!debug) dev.off()

}
