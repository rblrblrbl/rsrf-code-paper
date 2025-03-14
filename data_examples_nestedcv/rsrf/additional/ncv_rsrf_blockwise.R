#File for running RSRF on real dataset for a single run
#works in parallel (over trees)
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
  data_id_from_input <- as.character(args[[1]]) #e.g. chd_hd
  width_from_input <- as.integer( args[[2]]) #e.g. 45
  alg_from_input <- as.character( args[[3]])  #rsrf_nf
  seed_from_input <- as.integer( args[[4]] ) #e.g. 123, or "NA"
  block_size_from_input <- as.integer( args[[5]]) #5
  round_id_from_input <- as.integer( args[[6]] ) #e.g. 1 or 2
  setting_id <- as.integer( args[[7]])
  if( round_id_from_input == 1 ){
    round_id <- "round1"
  }
  if( round_id_from_input == 2){
    round_id <- "round2"
  }
  directory <- ""
  outputfile <- paste0( directory, "output/", "single_", 
                        data_id_from_input, "_", 
                        "j", job_id, "_",
                        alg_from_input, 
                        "_w",width_from_input, 
                        "_setting", setting_id, 
                        "_", round_id)
  progress_file <- paste0(submit_dir, "/progress_sindex",setting_id,"_j",job_id,".txt") 
  if( !is.na(seed_from_input) ){
    seed_val <- seed_from_input
  }else{
    stop("Seed missing or not correctly specified")
  }
} 
#helper function
simple_extension <- function(df){
  paste0( "o", df$outerfold_id, "_i", df$innerfold_id, "_mval", df$mtry_val)
}

#Get information
data_id <- data_id_from_input
width_value <- width_from_input
alg_value <- alg_from_input

#Read files
## Read dataset
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS")) #dataset
dataset_loaded <- dataset_from_rds$dataset
## Read fold information
folds_list <- readRDS(paste0("folds/","folds_cv_",data_id,"_",round_id,".RDS")) #list with fold infos
## Read setting
setting_id_char <- paste0("sindex",setting_id)
grid_loaded <- readRDS(paste0("manual_grid/grid_",data_id,".RDS") )
grid_use <- grid_loaded[setting_id_char,]                       
file_extension <- simple_extension( grid_use )
cat("Running for the following setting: ",file_extension, "\n",
  "   o: outerfold \n",
  "   i: innerfold \n",
  "   mval: mtry_value used in RSRF \n\n\n",file = progress_file, append=TRUE)

## set seed
#create a sequence of independent seeds (given the provided seed)
RNG_seq_generated <- rngtools::RNGseq(200, seed=seed_val)
#set the RNG to the specified setting_id
rngtools::setRNG( RNG_seq_generated[[ setting_id ]] )

## Take information from setup file
mvalue <- as.integer(grid_use$mtry_val)
outer_fold_number <- as.integer(grid_use$outerfold_id)
inner_fold_number <- as.integer(grid_use$innerfold_id)


#Data and response settings
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
outerfold_ids <- folds_list$outer_folds[[outer_fold_number]]
dataset <- dataset_loaded[-outerfold_ids,]
row.names(dataset) <- NULL
fold_ids <- folds_list$inner_folds_list[[outer_fold_number]]$folds
data_cv_train <- dataset[-fold_ids[[inner_fold_number]], ]
data_cv_test  <- dataset[ fold_ids[[inner_fold_number]], ]
row.names(data_cv_train) <- NULL
row.names(data_cv_test) <- NULL

## Parameter setting
#set parameters
if( alg_value %in% c("rsrf_nf","rsrf_nf_norep") ){
  mtry_mode_value <- "nothingfixed"
  include_CART_CART_value <- TRUE
  fixed_cart_cart_value <- FALSE
  mtry_cart_cart_value <- mvalue
  mtry_rsrf_step_value <- mvalue
  mtry_rsrf_step_random_value <- NULL
  if( alg_value == "rsrf_nf"){ replace_value <- TRUE }
  if( alg_value == "rsrf_nf_norep"){ replace_value <- FALSE }
}


if( alg_value %in% c("rsrf_af","rsrf_af_norep") ){
  mtry_mode_value <- "allfixed"
  include_CART_CART_value <- TRUE
  fixed_cart_cart_value <- TRUE
  mtry_cart_cart_value <- NULL
  mtry_rsrf_step_value <- mvalue
  mtry_rsrf_step_random_value <- mvalue
  if( alg_value == "rsrf_af"){ replace_value <- TRUE }
  if( alg_value == "rsrf_af_norep"){ replace_value <- FALSE }
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


## Run for this setting

blocksize <- block_size_from_input
if( !( parameter_list$num_trees$value %% blocksize == 0 ) ){
  stop("num_trees must be a multiple of blocksize.")
}

cat( "Running for sindex: ", setting_id_char," (", file_extension,")\n",
     "Using blocksize: ", blocksize,"\n",
     "(",parameter_list$num_trees$value/blocksize, " independent tasks -> these run in parallel.)\n\n",
     "-------------------------------------------- \n\n",
     file=progress_file, append =TRUE)

if(onCluster){
  registerDoParallel(cores=procs)
  mcoptions <- list(preschedule=FALSE)
  
  ptime <- system.time({
    cv_results <- foreach(j = icount(parameter_list$num_trees$value/blocksize), .options.multicore=mcoptions ) %dopar% {  
      cat("Starting for block no. ", j, "\n\n", file =progress_file, append=TRUE )
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
      cv_results <- list(prediction = predict_rsrf, sindex = setting_id_char, block_number = j )
      cat("Finishing block no. ", j, "\n\n", file =progress_file, append=TRUE )
      return( cv_results )
    }
  })[3]  
} else {
  
  #not on cluster
  set.seed(seed_val)
  ptime <- system.time({
    cv_results <- foreach(j = icount(parameter_list$num_trees$value/blocksize)) %do% {  
      cat("Starting for block no. ", j, "\n\n", file =progress_file, append=TRUE )
      
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
      cv_results <- list(prediction = predict_rsrf, sindex = setting_id_char, block_number = j )
      cat("Finishing block no. ", j, "\n\n", file =progress_file, append=TRUE )
      
      return( cv_results )
    }
  })[3]   
}

saveRDS( cv_results, file = paste0(outputfile,"_res_detail.RDS") ) 

predict_forest <- numeric(0)
for( l in seq(1,nrow(data_cv_test)) ){
  predict_forest[l] <- mean(  
    sapply( seq(1,parameter_list$num_trees$value / blocksize), function(i){return( cv_results[[i]]$prediction[l] )} ) 
  )  
}

results_fold <- list( mse = mean( (predict_forest - data_cv_test[,response] )^2 ),  sindex = setting_id_char )

cat("Results from CV for RSRF.\n\n",
    "Round:               ", round_id,"\n",
    "setting id (sindex): ", setting_id_char,"\n",
    "outerfold_id:        ",outer_fold_number,"\n",
    "innerfold_id:        ",inner_fold_number,"\n",
    "Dataset:      ", data_id,"\n",
    "seed:         ", seed_val, "\n\n",
    "RSRF-Setting.\n",
    "Variant:      ", alg_value, "\n",
    "mtry value:   ", mvalue, "\n",
    "width value:  ", width_value, "\n\n",
    "--------------------\n",
    "----   RESULTS  ----\n",
    "--------------------\n\n",
    "MSE:     ", results_fold$mse,"\n",
    file= paste0(outputfile,".txt")
)

## save parameter list and errors
all_res <- list( data_id = data_id,
                 round = round_id,
                 sindex = setting_id_char,
                 outerfold_id = outer_fold_number,
                 innerfold_id = inner_fold_number,
                 mtry = mvalue,
                 error_inner = results_fold$mse,
                 algorithm = alg_value,
                 width = width_value,
                 parameters = parameter_list,
                 seed = seed_val)

saveRDS(all_res, file = paste0(outputfile,".RDS") )              
