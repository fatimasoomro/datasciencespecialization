
## Useful: constructing data frame in different ways 
## http://www.r-statistics.com/2011/12/data-frame-objects-in-r-via-r-in-action/

complete <- function( directory, id){
	
		doDebug = FALSE
	    ## 'directory' is a character vector of length 1 indicating the location of the CSV files
        ## 'id' is an integer vector indicating the monitor ID numbers to be used
        ## Return the id of the sensor and the number of elements that were complete
	
		z<-formatC(id, width = 3, format = "d", flag = "0") 
		loopoverfiles = paste(directory,"/", z,".csv", sep="")
		if(doDebug){
			print(z)	
			print("Will look at the files ")
			print(loopoverfiles)
			print(" < ------ > ")	
			}

      	## store values in these vectors by vec<-c(vec, new_val) etc
      	## Then create the dataframe by data.frame(id=id_vector, ncomplete = ncomplete_vector)
      	id_vector<-vector()
      	ncomplete_vector<-vector()
      	
	total_rows	= 0
	good_rows = 0
	## loop over all csv files 
	mycounter = 1
	for (i in id){
		
			if(doDebug) print(loopoverfiles[mycounter])
			myfile <- read.csv(loopoverfiles[mycounter])
			total_rows  = total_rows + nrow(myfile)
			
			tmp <- subset(myfile, !is.na(sulfate) & !is.na(nitrate))
			good_rows = good_rows + nrow(tmp)

			id_vector<- c(id_vector, i)
			ncomplete_vector<- c(ncomplete_vector, nrow(tmp))
			
			if(doDebug) cat(i, " ", nrow(tmp),"\n", sep =" ")

			mycounter = mycounter+1
			} ## for i in id
			
			df<- data.frame(id=id_vector, nobs=ncomplete_vector)
			print(df)
			
}			

