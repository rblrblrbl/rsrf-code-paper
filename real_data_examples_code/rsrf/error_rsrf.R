#File for running RSRF on real dataset with k-fold cross-validation
#works in parallel
library(doParallel)
library(doRNG)
library(simpleRSRF)

onCluster <- TRUE

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
  width_from_input <- as.integer( args[[3]]) #e.g. 5
  mtry_from_input <- as.integer( args[[4]] ) #e.g. 3
  alg_from_input <- as.character( args[[5]])  #rsrf_nf / rsrf_af 
  nfolds_from_input <- as.integer( args[[6]] ) #e.g. 0
  seed_from_input <- as.integer( args[[7]] ) #e.g. 9
  directory <- ""
  outputfile_all_in_one  <- paste0( directory, "output/cv_", ifelse( is_test_from_input, "test_",""), "data_id_", data_id_from_input, "_jobID_", job_id, "_", alg_from_input, "_w",width_from_input,"_",mtry_from_input)
  outputfile_fold_information <-paste0( directory, "output/folds_info_cv_", ifelse( is_test_from_input, "test_",""), "data_id_", data_id_from_input, "_jobID_", job_id, "_", alg_from_input, "_w",width_from_input,"_",mtry_from_input)

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

### RSRF Settings ###
width_value <- width_from_input
alg_value <- alg_from_input
mtry_value <- mtry_from_input

if( alg_value == "rsrf_af" ){
  mtry_mode_value <- "allfixed"
  include_CART_CART_value <- TRUE
  fixed_cart_cart_value <- TRUE
  mtry_rsrf_step_random_value <- mtry_value
  mtry_rsrf_step_value <- mtry_value
  mtry_cart_cart_value <- NULL
}

if( alg_from_input == "rsrf_nf"){
  mtry_mode_value <- "nothingfixed"
  include_CART_CART_value <- TRUE
  fixed_cart_cart_value <- FALSE
  mtry_cart_cart_value <- mtry_value
  mtry_rsrf_step_value <- mtry_value
  mtry_rsrf_step_random_value <- NULL
}

### Make list of parameters
parameter_list <- list(
  mtry_mode = list(name = "mtry_mode", 
                   value = mtry_mode_value),
  
  rsrf_width = list(name = "rsrf_width",
                    value = width_value),
  
  mtry_rsrf_step = list(name = "mtry_rsrf_step",
                        value = mtry_rsrf_step_value),
  
  mtry_rsrf_step_random = list(name = "mtry_rsrf_step_random",
                        value = mtry_rsrf_step_random_value),
  
  mtry_cart_cart = list(name = "mtry_cart_cart", 
                        value = mtry_cart_cart_value),
  
  include_CART_CART = list(name = "include_CART_CART",
                           value= include_CART_CART_value ),
  
  fixed_cart_cart = list(name = "fixed_cart_cart",
                          value = fixed_cart_cart_value),
  
  rsrf_depth = list(name = "rsrf_depth",
                    value = 2),
  
  num_trees = list(name = "num_trees",
                   value = 100),
  
  min_node_size = list(name = "min_node_size",
                       value = 5),
  
  replace = list(name = "replace",
                 value = TRUE)
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

##possibly save folds
# saveRDS( list( seed = seed, nfolds = nfolds, folds  = fold_ids ), file=paste0(outputfile_fold_information,".RDS") )
# print("Saved information on CV folds.")

if(!onCluster) mcoptions <- NULL
ptime <- system.time({
cv_results <- foreach(j = icount(nfolds), .options.multicore=mcoptions ) %dopar% {
    data_cv_train <- dataset[-fold_ids[[j]], ]
    data_cv_test  <- dataset[ fold_ids[[j]], ]
    rsrf <- simpleRSRF( formula = formula,
                        data = data_cv_train,
                        num_threads = 1,
                        randomization = TRUE,
                        saveNodeInformation = FALSE,
                        num_trees = parameter_list$num_trees$value,
                        min_node_size = parameter_list$min_node_size$value,
                        replace = parameter_list$replace$value,
                        rsrf_width = parameter_list$rsrf_width$value,
                        mtry_mode = parameter_list$mtry_mode$value,
                        mtry_rsrf_step_random = parameter_list$mtry_rsrf_step_random$value,
                        mtry_rsrf_step = parameter_list$mtry_rsrf_step$value,
                        mtry_cart_cart = parameter_list$mtry_cart_cart$value,
                        fixed_cart_cart = parameter_list$fixed_cart_cart$value,
                        rsrf_depth = parameter_list$rsrf_depth$value,
                        include_CART_CART = parameter_list$include_CART_CART$value
    )
    predict_rsrf <- rsrf$predict( data_cv_test )
    mse_error <- mean( (predict_rsrf - data_cv_test[,response] )^2 )
    return( list(cv_error = mse_error ) )
  }
})[3]
## extract
cv_all_errors <- sapply( 1:nfolds, function(i){ return( cv_results[[i]]$cv_error ) })
cv_mean_error <- mean(cv_all_errors)
sd_error <- sd(cv_all_errors)

cat("Results from CV for RSRF.\n\n",
    "Dataset:      ", data_id,"\n",
    "seed:         ", seed, "\n\n",
    "RSRF-Setting.\n",
    "Variant:      ", alg_value, "\n",
    "mtry value:   ", mtry_value, "\n",
    "width value:  ", width_value, "\n\n",
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
                 width = width_value,
                 mtry = mtry_value,
                 parameters = parameter_list,
                 seed = seed,
                 folds = fold_ids )

saveRDS(all_res, file = paste0(outputfile_all_in_one,".RDS") )              
