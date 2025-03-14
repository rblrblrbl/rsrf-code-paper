#File for running RSRF on real dataset using nested CV, for a single outer-loop fold-ID
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
  data_id_from_input <- as.character(args[[1]]) #e.g. robot / robot_small
  width_from_input <- as.integer( args[[2]]) #e.g. 5
  alg_from_input <- as.character( args[[3]])  #rsrf_af & rsrf_nf
  outer_fold_number_from_input <- as.integer( args[[4]] ) #e.g. 1,2,3,4,5
  round_id_from_input <- as.character( args[[5]] ) #e.g. 1,2,3,4,5
  directory <- ""
  outputfile_all    <- paste0( directory, "output/", "res_using_best_", data_id_from_input, "_", "j", job_id, "_",alg_from_input, "_w",width_from_input, "_outer",outer_fold_number_from_input)
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
best_param_file <- paste0("results_inner/", data_id, "/",alg_value, "/best_param_", data_id, "_", alg_value, "_w", width_value, "_outerfold", outer_fold_number,"_",round_id,"_with_folds.RDS")
settings_from_file <- readRDS( best_param_file )

#Set best_parameters from file, set folds from file
outerfold_ids <- settings_from_file$outer_fold
parameter_list_best <- settings_from_file$parameters_best
seed <- settings_from_file$seed
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


### RSRF Settings ###
width_value <- width_from_input
alg_value <- alg_from_input

### In this file, setting mtry_values_inner is only needed for seed - settings, such that
### behaviour would be the same, as if started directly from file "nested_cv_rsrf_nf.R" with "run_with_best == TRUE"
# For looping through mtry values in inner-CV #
if( data_id %in% c("concrete","airfoil","robot","chd","abalone_enc")){
  mtry_values_inner <- 1:num_covariable
  mtry_values_inner_length <- length(mtry_values_inner)
  nfolds_inner <- 5
} else if (data_id %in% c("concrete_hd","airfoil_hd","robot_hd", "abalone_enc_hd")){
  mtry_values_inner <- c(10,20,30,40,50,num_covariable)
  mtry_values_inner_length <- length(mtry_values_inner)
  nfolds_inner <- 5
} else if (data_id %in% "robot_small"){
  mtry_values_inner <- c(2,3,4,5)
  mtry_values_inner_length <- length(mtry_values_inner)
  nfolds_inner <- 3
} else{
  stop("Invalid data id.")
}
### for reproducability
### Following chapter 5.2 of https://cran.r-project.org/web/packages/doRNG/vignettes/doRNG.pdf (work around for nested foreach loops)
### (as of 30/12/2024)
if( onCluster){
  registerDoParallel(cores = procs)
  rng <- RNGseq( mtry_values_inner_length * nfolds_inner + 1, seed)

  
  ### CV ###
  rngtools::setRNG(rng[[mtry_values_inner_length * nfolds_inner + 1]])
  #fix a seed for the final run. this is useful in case one wants to reproduce the result of the final run independently of the inner-CV search.
}
if(!onCluster){
  set.seed(seed)
}
  
#run on test fold with best setting
rsrf_best <- simpleRSRF( formula = formula,
                        data = dataset,
                        num_threads = 1,
                        randomization = TRUE,
                        saveNodeInformation = FALSE,
                        num_trees = parameter_list_best$num_trees$value,
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
predict_rsrf_best <- rsrf_best$predict( dataset_outer_test )
mse_error_best <- mean( (predict_rsrf_best - dataset_outer_test[,response] )^2 )
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
      "--------------------\n",
      "----   RESULTS  ----\n",
      "--------------------\n\n",
      "Round id:            ", round_id,
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
  
saveRDS(all_res, file = paste0(outputfile_all,"_with_folds.RDS") ) 
saveRDS(all_res_without_folds, file = paste0(outputfile_all,".RDS") )
