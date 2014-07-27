
pollutantmean <- function( directory, pollutant, id=1:332){
	
		doDebug=FALSE
		
	    ## 'directory' is a character vector of length 1 indicating the location of the CSV files
	    ## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
        ## 'id' is an integer vector indicating the monitor ID numbers to be used
        ## Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
	
		z<-formatC(id, width = 3, format = "d", flag = "0") 
		if(doDebug) print (is.character(z))
		loopoverfiles = paste(directory, "/", z,".csv", sep="")
		if(doDebug){
			print(z)
			print("Will look at the files ")
			print(loopoverfiles)
			print(" < ------ > ")	
			}
		subsetted_frame <-data.frame()
		total_rows = 0
		good_rows = 0
		my_new_frame = TRUE
		
		## loop over all csv files 
		mycounter = 1
		
		for (i in id){
			
			if(doDebug)  print(loopoverfiles[mycounter])
			myfile <- read.csv(loopoverfiles[mycounter])
			total_rows  = total_rows + nrow(myfile)
			
			if (nrow(subsetted_frame)==0) my_new_frame = TRUE
			else my_new_frame = FALSE
			
			if(pollutant == "nitrate"){
				
				if(doDebug) print("looking for nitrate")
				tmp <- subset(myfile, !is.na(nitrate))# & !is.na(sulfate))
				good_rows = good_rows + nrow(tmp)
				
				if(my_new_frame) subsetted_frame <- tmp
				else 	subsetted_frame <- rbind(subsetted_frame, tmp)
				
						
			}
			else if (pollutant =="sulfate"){
				
				if(doDebug) print("looking for sulfate")
				tmp <- subset(myfile, !is.na(sulfate))# & !is.na(nitrate))
				good_rows = good_rows + nrow(tmp)
				
				if(my_new_frame) subsetted_frame <- tmp
				else 	subsetted_frame <- rbind(subsetted_frame, tmp)

			}
			else {
				return -1000.0
			}
			
			mycounter = mycounter+1
		} ## for i in id 
		
		column_fetch = -1
		for(i in 1:length(subsetted_frame))
		{
			if( colnames(subsetted_frame)[i] == pollutant){
					column_fetch = i
					if(doDebug) cat("will fetch column ", column_fetch , "corresponding to ",colnames(subsetted_frame)[i] ,"\n" )
					break
			}				
		}
		if(column_fetch <1) return -1000.
		
		if(doDebug) cat("total rows ", total_rows, "\n")
		if(doDebug) cat("good rows ", good_rows, "\n")	
		
		myvector <- subsetted_frame[,column_fetch]
		if(doDebug) cat("mean is ", format(mean(myvector, na.rm=TRUE), digits=4), "\n")
		else print (as.numeric((format(mean(myvector, na.rm=TRUE), digits=4))))
	
} ## function pollutantmean 
