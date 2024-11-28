#File for running EXTRATREES on real dataset with k-fold cross-validation
library(foreach)
library(iterators)
onCluster <- FALSE
if(!onCluster){
  #set this to the directory where the files are stored. in particular this needs to contain the folder
  # - datasets containing the datasets
  mydirectory <- ""
  setwd(mydirectory)
  args=(commandArgs(TRUE))
  #Use args when running from command line.
  if( length(args) > 0 ){
    data_id_from_input <- as.character(args[[1]]) #e.g. airfoil
    alg_from_input <- as.character(args[[2]]) #e.g. et_replace / et_norep_sf1
    is_test_from_input <- as.logical( args[[3]] ) #FALSE (test mode not implemented in this file)
    mtry_from_input <- as.integer( args[[4]] ) #e.g. 7
    num_random_splits_from_input <- as.integer( args[[5]] ) #e.g. 10
    ntrees_from_input <- as.integer( args[[6]] ) #e.g. 500
    seed_from_input <- as.integer( args[[7]] ) #e.g. 123
    nfolds_from_input <- as.integer( args[[8]] ) #e.g. 5 or 10
  }else{
    #Used if no args are provided,convenient for usage within RStudio
    mtry_from_input <- 5
    alg_from_input <- "et_replace"
    data_id_from_input <- "airfoil"
    is_test_from_input <- FALSE
    num_random_splits_from_input <- 3
    seed_from_input <- 9
    nfolds_from_input <- 10
    ntrees_from_input <- 500
  }
  
  seed <- seed_from_input
  set.seed(seed)
  submit_dir <- getwd()
  
  job_id <- "local"
  #create directory for setting
  directory <- mydirectory
  tempname <- paste0(data_id_from_input,"_seed",seed_from_input,"_nfolds", nfolds_from_input)
  dest_directory <-paste0(directory,"/output_cv_local/et/", tempname, "/",alg_from_input)
  if(!dir.exists(dest_directory)){
    dir.create(dest_directory, recursive = TRUE )
  }
  
  if(data_id_from_input == "california_housing_div_hd"){
    data_id_small <- "chd_hd"
  } else {
      if(data_id_from_input =="abalone_all_hd"){
        data_id_small <- "ab_hd"
      } else {
        data_id_small <- ifelse( data_id_from_input == "california_housing_div", "chd", data_id_from_input)
      }
  }
  #output file name
  outputfile_all_in_one   <- paste0(  ifelse( is_test_from_input, "test_",""), "CV_", data_id_small, "_", alg_from_input, "_mtry_",mtry_from_input, "_ntrees_", ntrees_from_input, "_randomsplit_", num_random_splits_from_input)
}

### MAY BE CONVENIENT FOR USAGE ON A CLUSTER SYSTEM
# if(onCluster){
#   args=(commandArgs(TRUE))
#   job_id <- Sys.getenv("SLURM_JOB_ID")
#   submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
#   directory <- ""
#   if( length(args) > 0 ){
#     data_id_from_input <- as.character(args[[1]]) #e.g. airfoil
#     alg_from_input <- as.character(args[[2]]) #e.g. et_replace / et_norep_sf1
#     is_test_from_input <- as.logical( args[[3]] ) #FALSE
#     mtry_from_input <- as.integer( args[[4]] ) #e.g. 7
#     num_random_splits_from_input <- as.integer( args[[5]] ) #e.g. 10
#     ntrees_from_input <- as.integer( args[[6]] ) #e.g. 500
#     seed_from_input <- as.integer( args[[7]] ) #e.g. 123
#     nfolds_from_input <- as.integer( args[[8]] ) #e.g. 5 or 10
#   }else{
#     stop("args not supplied")
#   }
#   
#   seed <- seed_from_input
#   set.seed(seed)
#   submit_dir <- getwd()
#   dest_directory <-paste0(directory,"/output/")
# 
#   #shorter filenames
#   if(data_id_from_input == "california_housing_div_hd"){
#     data_id_small <- "chd_hd"
#   } else {
#     if(data_id_from_input =="abalone_all_hd"){
#       data_id_small <- "ab_hd"
#     } else {
#       data_id_small <- ifelse( data_id_from_input == "california_housing_div", "chd", data_id_from_input)
#     }
#   }
#   #output filename
#   outputfile_all_in_one   <- paste0(  ifelse( is_test_from_input, "test_",""), "CV_", data_id_small, "_", alg_from_input, "_mtry_",mtry_from_input, "_ntrees_", ntrees_from_input, "_randomsplit_", num_random_splits_from_input)
#   #outputfile_fold_information   <- paste0( directory, "/output_cv_local/fold_info_cv", ifelse( is_test_from_input, "test_",""), "CV_all_", data_id_from_input, "_", alg_from_input)
# }


### Setting of dataset ####
data_id <- data_id_from_input
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS"))
dataset <- dataset_from_rds$dataset
row.names(dataset) <- NULL #remove row names, not necessary

#test
isTest <- is_test_from_input
if( isTest ){
  stop("test mode not implemented")
}

#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
n_data <- nrow( dataset )
num_covariables <- dataset_from_rds$num_covariables

### CV Settings ###
nfolds <- nfolds_from_input
alg_value <- alg_from_input
mtry_value <- mtry_from_input
ntrees_value <- ntrees_from_input
num_random_splits_value <- num_random_splits_from_input 

if( alg_value == "et_replace" ){
### Make list of parameters
parameter_list <- list(
  mtry = list(name = "mtry", 
                   value = mtry_value),
  
  num.trees = list(name = "num.trees",
                   value = ntrees_value),
  
  num.random.splits = list(name = "num.random.splits",
                           value = num_random_splits_value),
  
  min.node.size = list(name = "min.node.size",
                       value = 5),
  
  replace = list(name = "replace",
                 value = TRUE),
  
  sample.fraction = list(name = "sample.fraction",
                         value = 1)
)
}


if( alg_value == "et_norep_sf1" ){
  ### Make list of parameters
  parameter_list <- list(
    mtry = list(name = "mtry", 
                value = mtry_value),
    
    num.trees = list(name = "num.trees",
                     value = ntrees_value),
    
    num.random.splits = list(name = "num.random.splits",
                             value = num_random_splits_value),
    
    min.node.size = list(name = "min.node.size",
                         value = 5),
    
    replace = list(name = "replace",
                   value = FALSE),
    
    sample.fraction = list(name = "sample.fraction",
                          value = 1)
    
  )
}

#### CV CODE ####
### Create Folds ###
neworder <- sample(1:n_data)
a <- round( seq(1,n_data, by = n_data/nfolds) )
b <- round( seq(n_data/nfolds ,n_data, by = n_data/nfolds) )
fold_ids <- lapply( 1:nfolds, function(i) neworder[seq(a[i],b[i]) ] )
for( j in 1:length(fold_ids)){
  print(paste0("Size of group ",j,": ",length(fold_ids[[j]])))
}

cv_results <- foreach(j = icount(nfolds) ) %do% {
    data_cv_train <- dataset[-fold_ids[[j]], ]
    data_cv_test  <- dataset[ fold_ids[[j]], ]
    rf <- ranger::ranger( formula = formula,
                        data = data_cv_train,
                        num.trees = parameter_list$num.trees$value,
                        min.node.size = parameter_list$min.node.size$value,
                        replace = parameter_list$replace$value,
                        mtry = parameter_list$mtry$value,
                        num.random.splits = parameter_list$num.random.splits$value,
                        sample.fraction = parameter_list$sample.fraction$value,
                        splitrule = "extratrees"
    )
    
    predict_rf <- predict(rf, data_cv_test)$predictions
    mse_error <- mean( (predict_rf -  data_cv_test[,response] )^2 )
    return( list(cv_error = mse_error ) )
}

## extract
cv_all_errors <- sapply( 1:nfolds, function(i){ return( cv_results[[i]]$cv_error ) })
cv_mean_error <- mean(cv_all_errors)
sd_error <- sd(cv_all_errors)

#save results
if(!onCluster){
  setwd(dest_directory)
  cat("Results from CV for EXTRATREES\n\n",
      "Dataset:            ", data_id,"\n",
      "seed:               ", seed, "\n\n",
      "algoritm:           ", alg_value, "\n\n",
      "EXTRA TREES-Setting.\n",
      "mtry:               ", mtry_value, "\n",
      "ntrees:             ", ntrees_value, "\n",
      "num.random.splits:  ", num_random_splits_value, "\n",
      "CV-Setting.\n",
      "no. folds", nfolds,"\n\n",
      "--------------------\n",
      "----   RESULTS  ----\n",
      "--------------------\n\n",
      "est. MSE:     ", cv_mean_error,"\n",
      "standard dev: ", sd_error,"\n\n\n",
      file= paste0(outputfile_all_in_one,".txt")
  )
  
  ## save parameter list and errors
  all_res <- list( data_id = data_id,
                   nfolds = nfolds,
                   cv_errors = cv_all_errors,
                   mean_error = cv_mean_error,
                   sd = sd_error,
                   algorithm = alg_value,
                   mtry = mtry_value,
                   ntrees = ntrees_value,
                   parameters = parameter_list,
                   seed = seed,
                   folds = fold_ids )
  saveRDS(all_res, file = paste0(outputfile_all_in_one,".RDS") )              
}

# if(onCluster){
#   cat("Results from CV for EXTRATREES\n\n",
#       "Dataset:            ", data_id,"\n",
#       "seed:               ", seed, "\n\n",
#       "algoritm:           ", alg_value, "\n\n",
#       "EXTRA TREES-Setting.\n",
#       "mtry:               ", mtry_value, "\n",
#       "ntrees:             ", ntrees_value, "\n",
#       "num.random.splits:  ", num_random_splits_value, "\n",
#       "CV-Setting.\n",
#       "no. folds", nfolds,"\n\n",
#       "--------------------\n",
#       "----   RESULTS  ----\n",
#       "--------------------\n\n",
#       "est. MSE:     ", cv_mean_error,"\n",
#       "standard dev: ", sd_error,"\n\n\n",
#       file= paste0("output/",outputfile_all_in_one,".txt")
#   )
#   
#   ## save parameter list and errors
#   all_res <- list( data_id = data_id,
#                    nfolds = nfolds,
#                    cv_errors = cv_all_errors,
#                    mean_error = cv_mean_error,
#                    sd = sd_error,
#                    algorithm = alg_value,
#                    mtry = mtry_value,
#                    ntrees = ntrees_value,
#                    parameters = parameter_list,
#                    seed = seed,
#                    folds = fold_ids )
#   saveRDS(all_res, file = paste0("output/",outputfile_all_in_one,".RDS") )              
# }
