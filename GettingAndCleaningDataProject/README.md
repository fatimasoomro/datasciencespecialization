Getting and cleaning data course project 
-----------------------------------------

For this project we are given a data set which contains some features recorded while subjects perform any of the 6 activities. 
The goal of the project is to create one R script called run_analysis.R that does the following. 
  * Merges the training and the test sets to create one data set.
  * Extracts only the measurements on the mean and standard deviation for each measurement. 
  * Uses descriptive activity names to name the activities in the data set
  * Appropriately labels the data set with descriptive variable names. 
  * Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

The first four are accomplished by the function mergedatasets in run_analysis.R It should be called as the following

     df<- mergedatasets(dirname, dodebug)

The dataframe it returns can be fed into the other function makeaverages like 

     makeaverages(df, dodebug)
which will write the desired dataframe in a csv file called "mysolutiondataframe.csv"

Both functions take "dodebug" as input, which, if set to true, will print some messages which can be helpful for debugging.
Further detailed comments are found in the .R file itself. 

A simple codebook is provided which maps the variable names in the original dataset to the ones in the new one. However, no significant change has been made to the variable names apart from the removal of special characters. The run_analysis will produce this file every time as well.