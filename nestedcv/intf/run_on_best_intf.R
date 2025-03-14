#File for running Interaction Forests using parameters obtained from inner-CV
library(diversityForest)

onCluster <- TRUE
if(!dir.exists("output")){
  dir.create("output")
}

if(onCluster){
  args=(commandArgs(TRUE))
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  job_id <- Sys.getenv("SLURM_JOB_ID")
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  data_id_from_input <- as.character(args[[1]]) #e.g. chd
  alg_from_input <- as.character( args[[2]])  #intf
  seed_from_input <- as.integer( args[[3]] ) #e.g. 123, or "NA"
  outerfold_id_from_input <- as.integer( args[[4]]) #e.g. 4
  round_id_from_input <- as.character( args[[5]] ) #e.g. round1 or round2

  directory <- ""
  
  ####
  outerfold_id <- outerfold_id_from_input
  round_id <- round_id_from_input
  if(!(round_id %in% c("round1","round2") ) ){
    stop("Invalid round id supplied")
  }
  
  outputfile <- paste0( directory, "output/", "single_", 
                        data_id_from_input, "_", 
                        alg_from_input,
                        "_o", outerfold_id,
                        "_", round_id,
                        "_j", job_id)
  progressfile <- paste0(submit_dir, "/progress_o",outerfold_id, "_",round_id, "_j",job_id,".txt") 
  
  if( !is.na(seed_from_input) ){
    seed_val <- seed_from_input
    set.seed(seed_val)
  }else{
    seed_val <- "no-seed"
  }
}

if(!onCluster){
  stop("not available")
}

### values from input
#Get data id
data_id <- data_id_from_input
alg_value <- alg_from_input

### Load and set dataset ####
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS")) #dataset
dataset_loaded <- dataset_from_rds$dataset
cat("Loaded dataset: ", data_id, "\n",file=progressfile, append = TRUE)

## Read best params file
best_param_file <- paste0("results_inner/", data_id, "/",alg_value, "/best_param_", data_id, "_", alg_value, "_o", outerfold_id,"_",round_id,".RDS")
settings_from_file <- readRDS( best_param_file )
cat("Loaded best_params from file: ", best_param_file, "\n",file=progressfile, append = TRUE)

#check if outerfold_id stored in loaded file matches provided outerfold_id (just for double-checking)
if(!(outerfold_id == settings_from_file$outer_fold_number) ){
  stop("Outerfold ID provided to R session does not match outerfold ID stored in corresponding file containing results from inner CV")
}

## Read fold information
folds_file <- paste0("folds/","folds_cv_",data_id,"_",round_id,".RDS")
folds_list <- readRDS(folds_file) #list with fold infos
cat("Loaded folds from file:", folds_file, "\n",file=progressfile, append = TRUE)

##
outerfold_ids <- folds_list$outer_folds[[outerfold_id]]
cat("Loaded outerfold_ids corresponding to outerfold no.", outerfold_id, "\n",file=progressfile, append = TRUE)
cat("First few outerfold ids: ",head(outerfold_ids), "\n",file=progressfile, append = TRUE)
cat("Seed was set to: ", seed_val, "\n\n", file=progressfile,append=TRUE)
## Take information from setup file

#Data and response settings
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))

dataset <- dataset_loaded
row.names(dataset) <- NULL
data_cv_train <- dataset[-outerfold_ids, ]
data_cv_test  <- dataset[outerfold_ids, ]
row.names(data_cv_train) <- NULL
row.names(data_cv_test) <- NULL
cat("Prepared dataset sucessfully.\n", file=progressfile,append=TRUE)

###
if( alg_value %in% c("intf","intf_norep")){
  cat("Setting up parameters (INTF).\n",file=progressfile, append = TRUE)
  
  ### Make list of parameters
  parameter_list <- settings_from_file$parameters_best 
  cat("Loaded parameter list for INTF from file:",
      best_param_file,"\n\n",
      file=progressfile, append = TRUE)
  
} else {
  stop("algorithm not supported")
}


cat( "Running for Outerfold: ", outerfold_id,"\n",
     "Round ID:", round_id, "\n",
     "npairs (from inner CV tuning) ", parameter_list$npairs$value, "\n",
     "------------------------------------------- \n\n",
     file=progressfile, append =TRUE)

intf <- interactionfor(formula = formula,
                        data = data_cv_train,
                        num.trees = parameter_list$num.trees$value,
                        min.node.size = parameter_list$min.node.size$value,
                        replace = parameter_list$replace$value,
                        npairs = parameter_list$npairs$value,
                        num.threads = parameter_list$num.threads$value,
                        importance = parameter_list$importance$value)


predict_intf <- predict(intf, data_cv_test)$predictions
mse_error <- mean( (predict_intf -  data_cv_test[,response] )^2 )
cat("Finishing.\n", file=progressfile, append = TRUE)


## SAVE RESULTS

cat("Results from CV for INTF (Single run) \n\n",
    "Round:               ",round_id,"\n",
    "outerfold_id:        ",outerfold_id,"\n",
    "Dataset:      ", data_id,"\n",
    "seed:         ", seed_val, "\n\n",
    "INTF-Setting.\n",
    "Algorithm:    ", alg_value, "\n",
    "npairs value: ", parameter_list$npairs$value, "\n",
    "num_trees:    ", parameter_list$num.trees$value, "\n\n",
    "--------------------\n",
    "----   RESULTS  ----\n",
    "--------------------\n\n",
    "MSE (inner):  ", mse_error,"\n",
    file= paste0(outputfile,".txt")
)

cat("Results from CV for INTF (Single run) \n\n",
    "Round:               ", round_id,"\n",
    "outerfold_id:        ",outerfold_id,"\n",
    "Dataset:      ", data_id,"\n",
    "seed:         ", seed_val, "\n\n",
    "INTF-Setting.\n",
    "Algorithm:    ", alg_value, "\n",
    "npairs value: ", parameter_list$npairs$value, "\n",
    "num_trees:    ", parameter_list$num.trees$value, "\n\n",
    "--------------------\n",
    "----   RESULTS  ----\n",
    "--------------------\n\n",
    "MSE (inner):  ", mse_error,"\n","\n\n",
    "(These resuls are also stored in the output file.)\n\n",
    file= progressfile,append=TRUE)

## save parameter list and errors
all_res <- list( data_id = data_id,
                 round = round_id,
                 outerfold_id = outerfold_id,
                 error = mse_error,
                 algorithm = alg_value,
                 npairs = parameter_list$npairs$value,
                 parameters = parameter_list,
                 seed = seed_val)

saveRDS(all_res, file = paste0(outputfile,".RDS") )              




