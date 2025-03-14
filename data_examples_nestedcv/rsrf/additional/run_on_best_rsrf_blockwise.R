#File for running RSRF given best parameters from inner cv, for a single outer-loop fold-ID
#parallelizes over trees
library(doParallel)
library(doRNG)
library(simpleRSRF)

onCluster <- TRUE

if(!dir.exists("output")){
  dir.create("output")
}

if(onCluster){
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  args=(commandArgs(TRUE))
  job_id <- Sys.getenv("SLURM_JOB_ID")
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  data_id_from_input <- as.character(args[[1]]) #e.g. robot / robot_small
  width_from_input <- as.integer( args[[2]]) #e.g. 5
  alg_from_input <- as.character( args[[3]])  #rsrf_af & rsrf_nf
  outer_fold_number_from_input <- as.integer( args[[4]] ) #e.g. 1,2,3,4,5
  round_id_from_input <- as.character( args[[5]] ) #e.g. 1,2,3,4,5
  seed_from_input <- as.integer( args[[6]] )
  block_size_from_input <- as.integer( args[[7]])
  progress_file <- paste0(submit_dir, "/progress_j",job_id,".txt") 
  directory <- ""
  outputfile_all  <- paste0( directory, "output/", "res_using_best_", data_id_from_input, "_", "j", job_id, "_",alg_from_input, "_w",width_from_input, "_outer",outer_fold_number_from_input)
}
# 
if(!onCluster){
  stop("not available")
}
#
round_id <- round_id_from_input

#Get data id
data_id <- data_id_from_input
outer_fold_number <- outer_fold_number_from_input
alg_value <- alg_from_input
width_value <- width_from_input
#Read files
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS")) #dataset
best_param_file <- paste0("results_inner/", data_id, "/",alg_value, "/best_param_", data_id, "_", alg_value, "_w", width_value, "_outerfold", outer_fold_number,"_",round_id,"_with_folds_bws.RDS")
settings_from_file <- readRDS( best_param_file )

#Set best_parameters from file, set folds from file
outerfold_ids <- settings_from_file$outer_fold
parameter_list_best <- settings_from_file$parameters_best
seed <- seed_from_input
if(outer_fold_number != settings_from_file$outer_fold_number ){
  stop("Error. Wrong fold stored in supplied file.")
}
print(paste0("Size of outer fold ",outer_fold_number,": ",length(outerfold_ids) ) )



### Setting of dataset ####
dataset_loaded <- dataset_from_rds$dataset
row.names(dataset_loaded) <- NULL #remove row names
dataset <- dataset_loaded[-outerfold_ids,]
dataset_outer_test <- dataset_loaded[outerfold_ids,]
row.names(dataset) <- NULL
row.names(dataset_outer_test) <- NULL

if( nrow( dataset ) == dataset_from_rds$n_data ){
  stop("Error. The dataset needs to be the orginal dataset WITHOUT the current outer fold.")
}

#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
n_data <- nrow( dataset )
num_covariable <- dataset_from_rds$num_covariable


if( onCluster){
  registerDoParallel(cores = procs)
  set.seed(seed)
}

## Run for this setting

blocksize <- block_size_from_input
if( !( parameter_list_best$num_trees$value %% blocksize == 0 ) ){
  stop("num_trees must be a multiple of blocksize.")
}

ptime <- system.time({
  cv_results <- foreach(j = icount(parameter_list_best$num_trees$value/blocksize) ) %dorng% {  
    cat("Starting for block no. ", j, "\n\n", file =progress_file, append=TRUE )
    rsrf <- simpleRSRF( formula = formula,
                        data = dataset,
                        num_threads = 1,
                        randomization = TRUE,
                        saveNodeInformation = FALSE,
                        num_trees = blocksize,
                        min_node_size = parameter_list_best$min_node_size$value,
                        replace = parameter_list_best$replace$value,
                        rsrf_width = parameter_list_best$rsrf_width$value,
                        mtry_mode = parameter_list_best$mtry_mode$value,
                        mtry_rsrf_step_random = parameter_list_best$mtry_rsrf_step_random$value,
                        mtry_rsrf_step = parameter_list_best$mtry_rsrf_step$value,
                        mtry_cart_cart = parameter_list_best$mtry_cart_cart$value,
                        fixed_cart_cart = parameter_list_best$fixed_cart_cart$value,
                        rsrf_depth = parameter_list_best$rsrf_depth$value,
                        include_CART_CART = parameter_list_best$include_CART_CART$value
    )
    predict_rsrf <- rsrf$predict( dataset_outer_test )
    cv_results <- list(prediction = predict_rsrf, block_number = j )
    cat("Finishing block no. ", j, "\n\n", file =progress_file, append=TRUE )
    return( cv_results )
  }
})[3]  

print("cv_results:")
print(cv_results)

predict_forest <- numeric(0)
for( l in seq(1,nrow(dataset_outer_test)) ){
  predict_forest[l] <- mean(  
    sapply( seq(1,parameter_list_best$num_trees$value / blocksize), function(i){return( cv_results[[i]]$prediction[l] )} ) 
  )  
}

# predict_forest <- mean( sapply( seq(1,parameter_list$num_trees$value / blocksize ), function(i){ return( cv_results[[i]]$prediction ) }) )
results_fold <- list( mse = mean( (predict_forest - dataset_outer_test[,response] )^2 ),  outerfold_id = outer_fold_number )
mse_error_best <- results_fold$mse
#save error
  
cat("Results from CV for RSRF on outer fold with best parameters.\n\n",
      "Outer fold id:       ", outer_fold_number,"\n",
      "Dataset:             ", data_id,"\n",
      "seed:                ", seed, "\n\n",
      "RSRF-Setting.\n",
      "Variant:             ", alg_value, "\n",
      "width value:         ", width_value, "\n\n",
      "CV-Setting.\n",
      "Outer fold id:       ", outer_fold_number,"\n\n",
      "Round id:            ", round_id,"\n",
      "--------------------\n",
      "----   RESULTS  ----\n",
      "--------------------\n\n",
      "MSE (outer fold):    ", mse_error_best,"\n\n", 
      file= paste0(outputfile_all,"_best.txt")
  )
  
  
## save parameter list and errors
all_res <- list( data_id = data_id,
                   outer_fold_number = outer_fold_number,
                   outer_fold = outerfold_ids,
                   error = mse_error_best,
                   algorithm = alg_value,
                   width = width_value,
                   parameters_best = parameter_list_best,
                   seed = seed
  )
  
all_res_without_folds <- list( data_id = data_id,
                   outer_fold_number = outer_fold_number,
                   error = mse_error_best,
                   algorithm = alg_value,
                   width = width_value,
                   parameters_best = parameter_list_best,
                   seed = seed
  )
  
saveRDS(all_res, file = paste0(outputfile_all,"best_with_folds.RDS") ) 
saveRDS(all_res_without_folds, file = paste0(outputfile_all,"best.RDS") )
