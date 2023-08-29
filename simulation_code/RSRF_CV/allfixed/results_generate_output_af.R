#### CONVERT ####
#Outputs error given RDS files in working directory
setwd("results_F6_af")
list_RDS_filenames <- list.files(pattern="*.RDS")
list_RDS_objects <- lapply(list_RDS_filenames, readRDS)
cat( "Combining Result files: \n", 
     paste(list_RDS_filenames, pattern = "\n"  ) )
errors <- NULL
for( i in 1:length( list_RDS_objects )){
  errors[[i]] <- sapply( list_RDS_objects[[i]] , function( lst ) return( lst$error ) )
}
errors <- Reduce( c, errors)
cat( "----- Results (Paper) ------", "\n", "ERROR: ", mean(errors), "\n",
     "SD:    ", sd(errors) )

####
setwd("../results")
list_RDS_filenames <- list.files(pattern="*.RDS")

if( length( list_RDS_filenames ) < 10 ){
  stop("Not all filenames are available yet. Make sure all simulations from file run_rsrf_af_F6 have been made. Files in results should be as in folder results_F6.")
}

list_RDS_objects <- lapply(list_RDS_filenames, readRDS)
cat( "Combining Result files: \n", 
     paste(list_RDS_filenames, pattern = "\n"  ) )
errors <- NULL
for( i in 1:length( list_RDS_objects )){
  errors[[i]] <- sapply( list_RDS_objects[[i]] , function( lst ) return( lst$error ) )
}
errors <- Reduce( c, errors)
cat( "----- Results (Paper) ------", "\n", "ERROR: ", mean(errors), "\n",
     "SD:    ", sd(errors) )