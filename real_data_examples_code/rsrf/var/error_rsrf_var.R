#File for running RSRF on real dataset with k-fold cross-validation
#This is a variant of error_rsrf.R, but only simulates for one of the CV-folds at a time
#Parellizes over trees
#This was used for the examples "california_housing_div_hd", "robot_hd" (for rsrf-af) and partly for "abalone_all_hd" (here only for w=30, mtry=58), because RSRF is not optimized for computational speed.

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
  data_id_from_input <- as.character(args[[1]]) #e.g. robot_hd
  is_test_from_input <- as.logical(args[[2]]) #e.g. FALSE
  width_from_input <- as.integer( args[[3]]) #e.g. 5
  mtry_from_input <- as.integer( args[[4]] ) #e.g. 3
  alg_from_input <- as.character( args[[5]])  #rsrf_af / rsrf_nf 
  nfolds_from_input <- as.integer( args[[6]] ) #e.g. 5
  seed_from_input <- as.integer( args[[7]] ) #e.g. 123
  fold_number_from_input <- as.integer( args[[8]]) #e.g. 1,2,3,4,5
  directory <- ""
  outputfile_all_in_one  <- paste0( directory, "output/cv_", ifelse( is_test_from_input, "test_",""), "data_id_", data_id_from_input, "_jobID_", job_id, "_", alg_from_input, "_w",width_from_input,"_",mtry_from_input, "_fold-no_",fold_number_from_input)

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
fold_ids <- readRDS(paste0("folds/","folds_",data_id,".RDS"))
dataset <- dataset_from_rds$dataset
row.names(dataset) <- NULL #remove row names

#test
isTest <- is_test_from_input
if( isTest ){
  stop("Test mode not implemented")
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
  
  #fixed parameters
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

### Fold ###
fold_number <- fold_number_from_input
current_fold <- fold_ids$folds[[fold_number]]
print(paste0("Size of group ",fold_number,": ",length(current_fold) ) )

data_cv_train <- dataset[-current_fold, ]
data_cv_test  <- dataset[ current_fold, ]

blocksize <- 5

ptime <- system.time({
    cv_results <- foreach(j = icount(parameter_list$num_trees$value/blocksize), .options.multicore=mcoptions ) %dopar% {  
    rsrf <- simpleRSRF( formula = formula,
                            data = data_cv_train,
                            num_threads = 1,
                            randomization = TRUE,
                            saveNodeInformation = FALSE,
                            num_trees = blocksize,
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
    cv_results <- list(prediction = predict_rsrf, fold_number = fold_number, block_number = j )
    return( cv_results )
    }
})[3]  

saveRDS( cv_results, file = paste0(outputfile_all_in_one,"_res_detail.RDS") ) 

predict_forest <- numeric(0)
for( l in seq(1,nrow(data_cv_test)) ){
  predict_forest[l] <- mean(  
    sapply( seq(1,parameter_list$num_trees$value / blocksize), function(i){return( cv_results[[i]]$prediction[l] )} ) 
    )  
}

results_fold <- list( mse = mean( (predict_forest - data_cv_test[,response] )^2 ), fold_number = fold_number )

cat("Results from CV for RSRF.\n\n",
    "FOLD, no:     ", fold_number,"\n",
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
    "MSE:     ", results_fold$mse,"\n",
    file= paste0(outputfile_all_in_one,".txt")
)

## save parameter list and errors
all_res <- list( data_id = data_id,
                 nfolds = nfolds,
                 fold_number = fold_number,
                 cv_error = results_fold,
                 algorithm = alg_value,
                 width = width_value,
                 mtry = mtry_value,
                 parameters = parameter_list,
                 seed = seed,
                 folds = fold_ids )

saveRDS(all_res, file = paste0(outputfile_all_in_one,".RDS") )              
