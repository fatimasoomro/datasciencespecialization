plot1 <- function(data, debug){

  if(debug) cat("\n------------------   ------------------   ------------------   ------------------   ---------------- \n",
                " Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? \n ",
                " Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each \n ",
                " of the years 1999, 2002, 2005, and 2008. \n ",
                "------------------   ------------------   ------------------   ------------------   ---------------- \n \n ", sep="")

  lev <- levels( as.factor(data$year))

  mysum<- vector()
  nObs <- vector()
  
  # Loop over the years (lev object) and subset the dataframe and get the sum
  for (i in 1:length(lev)){
    
    myyear <- as.numeric(lev[i])
    if(debug) cat("Looking at year ", myyear, "\n", sep="")
    
    df<- subset(data, year==myyear)
    nObs[i] <- nrow(df)
    mysum[i] <- sum(df$Emissions, na.rm=TRUE)
    
    if(debug) cat("Found sum ", i, " = ", mysum[i]," using ", nObs[i], " observations \n", sep="")
  }

  if(!debug) png("plot1.png") ## open png devide to draw the plot onto 
  plot(mysum, main="Total emission of PM2.5 in the United States", ylab=paste("Emissions (tons)", sep=""), xlab="Year", axes=FALSE, ylim=c(0, max(mysum)) , xlim=c ( 0.5, length(lev)+0.5) )
  axis(1, at = c(1:length(mysum)), c(as.numeric(lev)) )
  axis(2, at = as.integer( seq(0, max(mysum), length.out = 4)),  as.integer(seq(0, max(mysum), length.out = 4) ))
  box()
  if(!debug) dev.off()

}
