
corr  <- function( directory, threshold=0){

            myMethod = "pearson"
            doDebug=FALSE
            runoverfiles = 1:332
            #if(!doDebug) runoverfiles = 1:332
	    ## 'directory' is a character vector of length 1 indicating the location of the CSV files
	    ## 'threshold' is a numeric vector of length 1, it is the number of complete observations that a location has to have  in order for the correlation to be computed
            ## Return: a vector of correlations for the monitors that meet the threshold criteria

            z<-formatC(runoverfiles , width = 3, format = "d", flag = "0") 
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

            corrVector <- vector()

            ## loop over all csv files         
            mycounter = 1
            for (i in runoverfiles){
              
              if(doDebug)  print(loopoverfiles[mycounter])

              myfile <- read.csv(loopoverfiles[mycounter])
              total_rows  = total_rows + nrow(myfile)
              
              tmp <- subset(myfile, !is.na(sulfate) & !is.na(nitrate))
              good_rows = good_rows + nrow(tmp)

              if(nrow(tmp) > threshold){

                ## get sulfate and nitrate as vectors 
                tmpsulfate<- as.vector(tmp$sulfate)
                tmpnitrate<- as.vector(tmp$nitrate)

                df<- data.frame(sulfate=tmpsulfate, nitrate=tmpnitrate)

                if(doDebug){
                  print(df)
                  print(nrow(df))
                }

                ## compute correlation 
                mycorr<- (cor(df, use="complete.obs", method=myMethod) )
                corrVector<- c(corrVector,      mycorr[1,2])

                if(doDebug) cat( mycorr[1,2], " here it is \n", is.vector(mycorr[1,2]), "\n",  sep="")
                 
               
              } ## if ( nrow(tmp) > threshold )
              
              mycounter = mycounter+1
            } ## for i in id

            if(doDebug) cat( "printing corrVector of length ", length(corrVector), "\n",  sep="")
              
            print (as.numeric(format(corrVector)))##, trim=TRUE, digits=5)))

	
} ## function corr
