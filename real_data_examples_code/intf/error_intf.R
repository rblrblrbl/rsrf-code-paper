#File for running INTF on real dataset with k-fold cross-validation
#works in parallel
library(doParallel)
library(doRNG)

onCluster <- FALSE

if(!dir.exists("output")){
  dir.create("output")
}

if(onCluster){
  # note: not working on windows
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  args=(commandArgs(TRUE))
  job_id <- Sys.getenv("SLURM_JOB_ID")
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  data_id_from_input <- as.character(args[[1]]) #e.g. airfoil
  is_test_from_input <- as.logical(args[[2]]) #e.g. FALSE
  alg_from_input <- as.character( args[[3]])  #intf / intf-replace
  npairs_from_input <- as.integer( args[[4]]) #e.g. 20
  ntrees_from_input <- as.integer( args[[5]] ) #500
  nfolds_from_input <- as.integer( args[[6]] ) #e.g. 10
  seed_from_input <- as.integer( args[[7]] ) #e.g. 2
  directory <- ""
  outputfile_all_in_one  <- paste0( directory, "output/cv_", ifelse( is_test_from_input, "test_",""), "data_id_", data_id_from_input, "_jobID_", job_id, "_", alg_from_input, "_ntrees",ntrees_from_input,"_npairs_",npairs_from_input)
  outputfile_fold_information <-paste0( directory, "output/folds_cv_", ifelse( is_test_from_input, "test_",""), "data_id_", data_id_from_input, "_jobID_", job_id)

  #setup parallel
  registerDoParallel(cores=procs)
  mcoptions <- list(preschedule=FALSE)
  #possibly set seed using DoRNG
  if( !is.na(seed_from_input) ){
    seed <- seed_from_input
    registerDoRNG( seed = seed, once = TRUE)
  }else{
    seed <- "no-seed"
  }
  
}
# 
# if(!onCluster){
#   directory <- "" #set to location where datasets folder is
#   npairs_from_input <- 20 #e.g. 20
#   ntrees_from_input <- 500
#   alg_from_input <- "intf"
#   data_id_from_input <- "airfoil"
#   is_test_from_input <- FALSE
#   seed_from_input <- 2
#   nfolds_from_input <- 10
#   seed <- seed_from_input
#   submit_dir <- getwd()
#   job_id <- "local"
#   mcoptions <- list()
#   outputfile_all_in_one   <- paste0( directory, "/output/", ifelse( is_test_from_input, "test_",""), "CV_all_", data_id_from_input, "_", alg_from_input)
#   outputfile_fold_information   <- paste0( directory, "/output/fold_info_cv", ifelse( is_test_from_input, "test_",""), "CV_all_", data_id_from_input, "_", alg_from_input)
# }


### Setting of dataset ####
data_id <- data_id_from_input
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS"))
dataset <- dataset_from_rds$dataset
row.names(dataset) <- NULL #remove row names

#test
isTest <- is_test_from_input
if( isTest ){
  dataset <- dataset[1:27,]
}

#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
n_data <- nrow( dataset )
num_covariables <- dataset_from_rds$num_covariables

### CV Settings ###
nfolds <- nfolds_from_input

alg_value <- alg_from_input
npairs_value <- npairs_from_input
ntrees_value <- ntrees_from_input

### INTF Settings ###
if( alg_value == "intf-replace" ){
  replace_value <- TRUE
}
if( alg_value == "intf" ){
  replace_value <- FALSE
}

### Make list of parameters
parameter_list <- list(
  #fixed parameters
  num.trees = list(name = "num.trees", 
                   value = ntrees_value),
  
  npairs = list(name = "npairs",
                value = npairs_value ),
  
  replace = list(name = "replace",
                 value = replace_value ),
  
  min.node.size = list(name = "min.node.size",
                       value = 5),
  
  num.threads = list(name = "num.threads", 
                     value = 1),
  
  importance = list(name = "importance", 
                    value = "none")
)

#### CV CODE ####

### Create Folds ###
neworder <- sample(1:n_data)
a <- round( seq(1,n_data, by = n_data/nfolds) )
b <- round( seq(n_data/nfolds ,n_data, by = n_data/nfolds) )
fold_ids <- lapply( 1:nfolds, function(i) neworder[seq(a[i],b[i]) ] )
for( j in 1:length(fold_ids)){
  print(paste0("Size of group ",j,": ",length(fold_ids[[j]])))
}

ptime <- system.time({
cv_results <- foreach(j = icount(nfolds), .options.multicore=mcoptions ) %dopar% {
    data_cv_train <- dataset[-fold_ids[[j]], ]
    data_cv_test  <- dataset[ fold_ids[[j]], ]
        intf <- diversityForest::interactionfor( formula = formula,
                          data = data_cv_train,
                          num.trees = parameter_list$num.trees$value,
                          min.node.size = parameter_list$min.node.size$value,
                          replace = parameter_list$replace$value,
                          npairs = parameter_list$npairs$value,
                          num.threads = parameter_list$num.threads$value,
                          importance = parameter_list$importance$value)
    predict_intf <- predict(intf, data_cv_test)$predictions
    mse_error <- mean( (predict_intf -  data_cv_test[,response] )^2 )
    return( list(cv_error = mse_error ) )
  }
})[3]
## extract
cv_all_errors <- sapply( 1:nfolds, function(i){ return( cv_results[[i]]$cv_error ) })
cv_mean_error <- mean(cv_all_errors)
sd_error <- sd(cv_all_errors)

cat("Results from CV for INTF\n\n",
    "Dataset:      ", data_id,"\n",
    "seed:         ", seed, "\n\n",
    "Setting.\n",
    "Variant:      ", alg_value, "\n",
    "npairs:       ", npairs_value, "\n",
    "ntrees:       ", ntrees_value, "\n\n",
    "(replace:      ", replace_value, ")\n\n",
    "CV-Setting.\n",
    "no. folds", nfolds,"\n\n",
    "--------------------\n",
    "----   RESULTS  ----\n",
    "--------------------\n\n",
    "est. MSE:     ", cv_mean_error,"\n",
    "standard dev: ", sd_error,"\n\n\n",
    sprintf('Parallel time using doParallel on %d workers: %f\n', getDoParWorkers(), ptime),  "\n",
    "   ... in hours:", ptime/3600, "\n",
    file= paste0(outputfile_all_in_one,".txt")
)

## save parameter list and errors
all_res <- list( data_id = data_id,
                 nfolds = nfolds,
                 cv_errors = cv_all_errors,
                 mean_error = cv_mean_error,
                 sd = sd_error,
                 algorithm = alg_value,
                 npairs = npairs_value,
                 ntrees = ntrees_value,
                 replace = replace_value,
                 parameters = parameter_list,
                 seed = seed,
                 folds = fold_ids )

saveRDS(all_res, file = paste0(outputfile_all_in_one,".RDS") )              
