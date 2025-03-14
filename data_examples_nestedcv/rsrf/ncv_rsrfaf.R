#File for running RSRF (af) on real dataset using nested CV
#for a single outer-loop fold-ID
#with inner k-fold cross-validation
#works in parallel using doParallel, foreach
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
  data_id_from_input <- as.character(args[[1]]) #e.g. airfoil
  is_test_from_input <- as.logical(args[[2]]) #e.g. FALSE
  width_from_input <- as.integer( args[[3]]) #e.g. 30
  alg_from_input <- as.character( args[[4]])  #rsrf_af or rsrf_af_norep
  outer_fold_number_from_input <- as.integer( args[[5]] ) #e.g. 1,2,3,4,5
  run_with_best_from_input <- as.logical( args[[6]] ) #e.g. TRUE / FALSE
  #if true, the method will be trained and tested on the outer fold with the parameters found from inner cv loop
  #if false, the best parameters found in the inner loop will only be stored in an .RDS file
  seed_from_input <- as.integer( args[[7]] ) #e.g. 123, or "NA"
  round_id <- as.integer( args[[8]] ) #e.g. 1 or 2
  if( round_id == 1 ){
    round_id <- "_round1"
    round_id_out <- "round: 1"
  }
  if( round_id == 2){
    round_id <- "_round2"
    round_id_out <- "round: 2"
  }
  directory <- ""
  outputfile_all    <- paste0( directory, "output/", ifelse( is_test_from_input, "test_",""), "nested_cv_", data_id_from_input, "_", "j", job_id, "_",alg_from_input, "_w",width_from_input, "_outerfold",outer_fold_number_from_input, round_id)
  outputfile_best   <- paste0( directory, "output/", ifelse( is_test_from_input, "test_",""), "best_param_", data_id_from_input, "_", "j", job_id, "_", alg_from_input, "_w",width_from_input, "_outerfold",outer_fold_number_from_input, round_id)
  if( !is.na(seed_from_input) ){
    seed <- seed_from_input
  }else{
    seed <- "no-seed"
  }
}

#either true or false. decides if only inner CV search for the given outer_cv index shall be executed (FALSE) or if, additionally, the algorithm with best parameters shall be executed on the outer cv fold (TRUE)
run_with_best <- run_with_best_from_input #

#Get data id
data_id <- data_id_from_input
outer_fold_number <- outer_fold_number_from_input

#Read files
folds_list <- readRDS(paste0("folds/","folds_cv_",data_id,round_id,".RDS")) #list with fold infos
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS")) #dataset

### Get outer fold ID ###
if( folds_list$nfolds_outer < outer_fold_number ){
  stop("Outer fold no. is too large. This fold does not exist.")
}
outerfold_ids <- folds_list$outer_folds[[outer_fold_number]]
print(paste0("Size of outer fold ",outer_fold_number,": ",length(outerfold_ids) ) )

### Setting of dataset ####
dataset_loaded <- dataset_from_rds$dataset
row.names(dataset_loaded) <- NULL #remove row names
dataset <- dataset_loaded[-outerfold_ids,]
dataset_outer_test <- dataset_loaded[outerfold_ids,]
row.names(dataset) <- NULL
row.names(dataset_outer_test) <- NULL

#test
isTest <- is_test_from_input
if( isTest ){
  print("Using dataset of reduced sample size 47 in inner cv.")
  dataset <- dataset[1:47,]
}

#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
n_data <- nrow( dataset )
num_covariable <- dataset_from_rds$num_covariable

### Inner CV Settings ###
### Get inner fold list ###
### Attention IDs for the inner folds refer to the IDs for the SUBSET of the dataset, where outer_folds are already REMOVED
nfolds_inner <- folds_list$nfolds_inner
fold_ids <- folds_list$inner_folds_list[[outer_fold_number]]$folds
for( j in 1:length(fold_ids)){
  print(paste0("Size of inner fold ",j,": ",length(fold_ids[[j]])))
}

### RSRF Settings ###
width_value <- width_from_input
alg_value <- alg_from_input

# For looping through mtry values in inner-CV #
if( data_id %in% c("concrete","airfoil","abalone","robot","chd","abalone_enc")){
  mtry_values_inner <- 1:num_covariable
  mtry_values_inner_length <- length(mtry_values_inner)
} else if (data_id %in% c("concrete_hd","airfoil_hd","robot_hd", "chd_hd", "abalone_enc_hd")){
  mtry_values_inner <- c(10,20,30,40,50,num_covariable)
  mtry_values_inner_length <- length(mtry_values_inner)
} else if (data_id %in% "robot_small"){
  mtry_values_inner <- c(2,3,4,5)
  mtry_values_inner_length <- length(mtry_values_inner)
} else{
  stop("Invalid data id.")
}

if( alg_value %in% c("rsrf_nf","rsrf_nf_norep" ) ){
  stop("This file is for simulation for rsrf_af/rsrf_af_norep.")
}

if( alg_from_input %in% c("rsrf_af","rsrf_af_norep") ){
  mtry_mode_value <- "allfixed"
  include_CART_CART_value <- TRUE
  fixed_cart_cart_value <- TRUE
  mtry_cart_cart_value <- NULL
  mtry_rsrf_step_value <- "to_tune"
  mtry_rsrf_step_random_value <-"to_tune"
  if( alg_from_input == "rsrf_af"){ replace_value <- TRUE }
  if( alg_from_input == "rsrf_af_norep"){ replace_value <- FALSE }
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
  
  replace = list(name = "replace",
                 value = replace_value),
  
  #fixed parameters
  rsrf_depth = list(name = "rsrf_depth",
                    value = 2),
  
  num_trees = list(name = "num_trees",
                   value = 100),
  
  min_node_size = list(name = "min_node_size",
                       value = 5)
  

)

cat("Progress for Job: ", job_id, "\n",
    "Running for outer fold number: ", outer_fold_number,"\n",
    "Data id:                       ", data_id, "\n\n",
    "--------------- PROGRESS -------------\n\n",
    file = paste0(submit_dir, "/progress_",job_id,".txt") )


### for reproducability
### Following chapter 5.2 of https://cran.r-project.org/web/packages/doRNG/vignettes/doRNG.pdf (work around for nested foreach loops)
### (as of 30/12/2024)
if( onCluster){
  registerDoParallel(cores = procs)
  rng <- RNGseq( mtry_values_inner_length * nfolds_inner + 1, seed)
}

### CV ###
if(onCluster){
ptime <- system.time({
  res_matrix <- foreach(j = icount(nfolds_inner), .combine = 'cbind' ) %:% #outer loop mit cbind: wird spalte
    foreach( m_index = icount(mtry_values_inner_length), r=rng[(j-1)*mtry_values_inner_length + 1:mtry_values_inner_length], .combine='c') %dopar% { #inner loop wird dann zeile
      
      ### For reproducability  # set RNG seed
      rngtools::setRNG(r)
      
      ##
      cat("Starting.. fold: ", j,", m_index: ",m_index," (corresponds to mtry: ", mtry_values_inner[m_index],")\n",
          file= paste0(submit_dir,"/progress_",job_id,".txt"), append = TRUE )
      
      parameter_list_current <- parameter_list
      parameter_list_current$mtry_rsrf_step_random$value <- mtry_values_inner[m_index] #for rsrf-allfixed mode
      parameter_list_current$mtry_rsrf_step$value <- mtry_values_inner[m_index] #for rsrf-allfixed mode
      data_cv_train <- dataset[-fold_ids[[j]], ]
      data_cv_test  <- dataset[ fold_ids[[j]], ]
      rsrf <- simpleRSRF( formula = formula,
                          data = data_cv_train,
                          num_threads = 1,
                          randomization = TRUE,
                          saveNodeInformation = FALSE,
                          num_trees = parameter_list_current$num_trees$value,
                          min_node_size = parameter_list_current$min_node_size$value,
                          replace = parameter_list_current$replace$value,
                          rsrf_width = parameter_list_current$rsrf_width$value,
                          mtry_mode = parameter_list_current$mtry_mode$value,
                          mtry_rsrf_step_random = parameter_list_current$mtry_rsrf_step_random$value,
                          mtry_rsrf_step = parameter_list_current$mtry_rsrf_step$value,
                          mtry_cart_cart = parameter_list_current$mtry_cart_cart$value,
                          fixed_cart_cart = parameter_list_current$fixed_cart_cart$value,
                          rsrf_depth = parameter_list_current$rsrf_depth$value,
                          include_CART_CART = parameter_list_current$include_CART_CART$value
      )
      predict_rsrf <- rsrf$predict( data_cv_test )
      mse_error <- mean( (predict_rsrf - data_cv_test[,response] )^2 )
      
      cat("Finishing fold: ", j,", m_index: ",m_index,"\n", file = paste0(submit_dir,"/progress_",job_id,".txt"), append = TRUE )
      
      return( mse_error )
    }
  colnames(res_matrix) <- paste0("fold",seq(1:nfolds_inner))
  rownames(res_matrix) <- paste0("m_index.",seq(1:mtry_values_inner_length))
})[3]
rngtools::setRNG(rng[[mtry_values_inner_length * nfolds_inner + 1]])
#fix a seed for the final run
}

#find best
cv_inner_errors <- rowMeans(res_matrix)
m_index_best <- which.min( cv_inner_errors )#get index of minimum. In unlikely case of two minima, will return the first
#save best setting 
parameter_list_best <- parameter_list
mtry_best <- mtry_values_inner[m_index_best]
parameter_list_best$mtry_rsrf_step_random$value <- mtry_best
parameter_list_best$mtry_rsrf_step$value <- mtry_best


saveRDS( 
  list( parameters_best = parameter_list_best,
        outer_fold_number = outer_fold_number, 
        outer_fold = outerfold_ids,
        inner_folds = fold_ids,
        nfolds_inner = nfolds_inner,
        seed = seed,
        res_matrix = res_matrix,
        round_id = round_id_out), paste0(outputfile_best,"_with_folds.RDS")
)

saveRDS( 
  list( parameters_best = parameter_list_best,
        outer_fold_number = outer_fold_number, 
        nfolds_inner = nfolds_inner,
        seed = seed,
        res_matrix = res_matrix,
        round_id = round_id_out ), paste0(outputfile_best,".RDS")
)


# given the best parameters, run on dataset from outer_cv and evaluate on test set from outer cv
if(!run_with_best){
  cat("Not running with optimal parameters found in inner cv because run_with_best is set to false. You can do this seperately.\n", file = paste0(submit_dir,"/progress_",job_id,".txt"), append = TRUE )
}
if(run_with_best){
  cat("Running with optimal parameters found in inner cv.\n", file = paste0(submit_dir,"/progress_",job_id,".txt"), append = TRUE )
}

if( run_with_best ){
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

cat("Results from CV for RSRF.\n\n",
    "Round_id:            ", round_id_out,"\n",
    "Outer fold id:       ", outer_fold_number,"\n",
    "Dataset:             ", data_id,"\n",
    "seed:                ", seed, "\n\n",
    "RSRF-Setting.\n",
    "Variant:             ", alg_value, "\n",
    "mtry values (tried): ",mtry_values_inner,"\n",
    "mtry value (best)  : ",mtry_best, "\n",
    "width value:         ", width_value, "\n\n",
    "CV-Setting.\n",
    "no. inner folds      ", nfolds_inner,"\n",
    "Outer fold id:       ", outer_fold_number,"\n\n",
    "--------------------\n",
    "----   RESULTS  ----\n",
    "--------------------\n\n",
    "MSE (outer fold):    ", mse_error_best,"\n\n", 
    sprintf('Parallel time using doParallel on %d workers: %f\n', getDoParWorkers(), ptime),  "\n",
    "   ... in hours:", ptime/3600, "\n",
    file= paste0(outputfile_all,".txt")
)


## save parameter list and errors
all_res <- list( data_id = data_id,
                 round_id = round_id_out,
                 outer_fold_number = outer_fold_number,
                 outer_fold = outerfold_ids,
                 inner_folds = fold_ids,
                 nfolds_inner = nfolds_inner,
                 error = mse_error_best,
                 algorithm = alg_value,
                 width = width_value,
                 mtry_best = mtry_best,
                 parameters_best = parameter_list_best,
                 seed = seed
)

all_res_without_folds <- list( data_id = data_id,
                 round_id = round_id_out,
                 outer_fold_number = outer_fold_number,
                 nfolds_inner = nfolds_inner,
                 error = mse_error_best,
                 algorithm = alg_value,
                 width = width_value,
                 mtry_best = mtry_best,
                 parameters_best = parameter_list_best,
                 seed = seed
)

saveRDS(all_res, file = paste0(outputfile_all,"_with_folds.RDS") )        
saveRDS(all_res_without_folds, file = paste0(outputfile_all,".RDS") )              

}