#### CONVERT ####
#Outputs error given RDS files in working directory
print("This file calculates the results for ALL .RDS files in folder INTF_CV/results")
print("Make sure that only those .RDS files are included for which you want to calculate the results. e.g. only those for model A4")
####
setwd("../results")
list_RDS_filenames <- list.files(pattern="*.RDS")
list_RDS_objects <- lapply(list_RDS_filenames, readRDS)
cat( "Combining Result files: \n", 
     paste(list_RDS_filenames, pattern = "\n"  ) )
errors <- NULL
for( i in 1:length( list_RDS_objects )){
  errors[[i]] <- sapply( list_RDS_objects[[i]] , function( lst ) return( lst$error ) )
}
errors <- Reduce( c, errors)
cat( "-------- Results  --------", "\n", "ERROR: ", mean(errors), "\n",
     "SD:    ", sd(errors) )