###HELPER file to prepare output files that were generated from running testerror_cv_intf.R, test_error_cv_rf.R etc.
readallRDS <- function(folder_name,setwd_after = ".."){
  setwd(folder_name)
  list_RDS_filenames <- list.files(pattern="*.RDS")
  print(list_RDS_filenames)
  list_RDS_objects <- lapply(list_RDS_filenames, readRDS)
  setwd(setwd_after)
  return( list_RDS_objects )
}

### converts the output RDS file from testerror_cv.R into a data frame
extract_small <- function(res_cv){
  a <- data.frame( cv_error = res_cv$cv_errors, 
                   fold = seq(1:res_cv$nfolds),
                   algorithm = res_cv$algorithm)
  #FOR RSRF
  if( res_cv$algorithm %in% c("rsrf_af","rsrf_nf")){
    a$mtry <- res_cv$mtry
    a$width <- res_cv$width
    a$cv_error_mean <- res_cv$mean_error
    a$cv_sd <- res_cv$sd
  }
  #FOR RF
  if( res_cv$algorithm == "rf"){
    a$mtry <- res_cv$mtry
    a$cv_error_mean <- res_cv$mean_error
    a$cv_sd <- res_cv$sd
  }
  
  #FOR INTF
  if( res_cv$algorithm == "intf" || res_cv$algorithm == "intf-replace"){
    a$npairs <- res_cv$npairs
    a$ntrees <- res_cv$ntrees
    a$replace <- res_cv$replace
    a$cv_error_mean <- res_cv$mean_error
    a$cv_sd <- res_cv$sd
  }
  
  # FOR ET
  if( res_cv$algorithm == "et_replace" || res_cv$algorithm == "et_norep_sf1" ){
    parameters_list <- res_cv$parameters
    for( j in 1:length(parameters_list)){
      a[,parameters_list[[j]]$name] <- parameters_list[[j]]$value
    }
    a$cv_error_mean <- res_cv$mean_error
    a$cv_sd <- res_cv$sd
  }
  
  return(a)
}

extract_full <- function(res_cv){
  parameters_list <- res_cv$parameters
  a <-data.frame( cv_error = res_cv$cv_errors, 
                  fold = seq(1:res_cv$nfolds) )
  length(parameters_list)
  for( j in 1:length(parameters_list)){
    a[,parameters_list[[j]]$name] <- parameters_list[[j]]$value
  }
  return(a)
}

#use this to combine all results .RDS found in a folder, will be combined into a single dataframe
get_df <- function(folder_name,setwd_after =".."){
  res <- readallRDS(folder_name, setwd_after = setwd_after)
  df <- do.call( rbind, lapply( res, extract_small) )
  return(df)
}

#use this to combine all results .RDS found in a folder, will be combined into a single dataframe
get_df_full <- function(folder_name,setwd_after =".."){
  res <- readallRDS(folder_name, setwd_after = setwd_after)
  df <- do.call( rbind, lapply( res, extract_full) )
  return(df)
}

#use this to get all results .RDS found in a folder and save into a list
get_df_seperate <- function(folder_name, setwd_after = ".."){
  res <- readallRDS( folder_name, setwd_after = setwd_after )
  df_list <- lapply( res, extract_small )
  return( df_list )
}

