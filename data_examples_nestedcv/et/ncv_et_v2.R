#File for running ET on real dataset using nested CV, for the specified outer fold IDs
#runs 5-fold inner cross-validation for this outer fold ID,
#and then runs the algorithm using the chosen parameters on the outer train/test split

library(foreach)
library(iterators)
library(ranger)

onCluster <- TRUE
if(!dir.exists("output")){
  dir.create("output")
}

if(onCluster){
  args=(commandArgs(TRUE))
  job_id <- Sys.getenv("SLURM_JOB_ID")
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  data_id_from_input <- as.character(args[[1]]) #e.g. airfoil
  alg_from_input <- as.character( args[[2]])  #et_replace #et_sf1
  ntrees_from_input <- as.integer( args[[3]] ) #500
  seed_from_input <- as.integer( args[[4]] ) #e.g. 123, or "NA"
  round_from_input <- as.character( args[[5]]) #either "round1" or "round2"
  jouter_from_input <- as.integer( args[[6]]) #index 1,2,3,4 or 5. Determines for which of the five outer folds, the file is run.
  directory <- ""
  outputfile_all    <- paste0( directory, "output/", "nested_cv_", data_id_from_input, "_", alg_from_input,"_j", job_id)
  outputfile_best   <- paste0( directory, "output/", "best_param_", data_id_from_input, "_", alg_from_input,"_j", job_id)
  progressfile <- paste0(submit_dir,"/progress_j",job_id,".txt")
  if( !is.na(seed_from_input) ){
    seed <- seed_from_input
    set.seed(seed)
  }else{
    seed <- "no-seed"
  }
}

if(!onCluster){
  stop("not available")
}

### values from input
alg_value <- alg_from_input
ntrees_value <- ntrees_from_input
#Get data id
data_id <- data_id_from_input

#Read files
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS")) #dataset

#decide which of the round (or both) shall be ran
if( !( round_from_input %in% c("both","round1","round2") ) ){
  stop("Invalid round-value supplied.")
} else{
  if( round_from_input == "both"){
    stop("Invalid.")
  } else{
    round_val <- round_from_input
  }
}

cat("Loaded folds.\n", file=progressfile, append = TRUE)



### Setting of dataset ####
dataset_loaded <- dataset_from_rds$dataset
row.names(dataset_loaded) <- NULL #remove row names
#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
num_covariable <- dataset_from_rds$num_covariable

cat("Loaded dataset: ", data_id, "\n",file=progressfile, append = TRUE)

round <- round_val
jouter <- jouter_from_input
#run for each of the round_vals
#Read files
folds_list <- readRDS(paste0("folds/","folds_cv_",data_id,"_",round,".RDS")) #list with fold infos
nfolds_outer <- folds_list$nfolds_outer
nfolds_inner <- folds_list$nfolds_inner
###
###EXTRATREES
if( alg_value %in% c("et_replace","et_sf1") ){
  ### For looping through mtry values in inner-CV #
  if( data_id %in% c("concrete","airfoil","abalone","robot","chd","abalone_fm","abalone_enc")){
    mtry_values_inner <- 1:num_covariable
    nsplits_values_inner <- 1:10
  } else if (data_id %in% c("concrete_hd","airfoil_hd", "abalone_hd", "robot_hd", "chd_hd", "abalone_fm_hd", "abalone_enc_hd")){
    mtry_values_inner <- seq(5,55,by=5)
    nsplits_values_inner <- 1:10
  } else if (data_id %in% "robot_small"){
    mtry_values_inner <- c(2,3,4,5)
    nsplits_values_inner <- 1:8
  } else{
    stop("Invalid data id.")
  }
  
  g_tune <- expand.grid( mtry_values_inner,nsplits_values_inner  )
  names(g_tune) <- c("mtry","num.random.splits")
  grid_size <- nrow(g_tune)
  
  cat("Values set up for tuning (ET), Values to tune are given by grid of: \n",
      "num.random.splits: ", nsplits_values_inner, "\n",
      "and mtry: ",mtry_values_inner, "\n",file=progressfile, "\n\n",
      "Size of grid: ", grid_size, "\n",append = TRUE)
  
  if( alg_value == "et_replace"){
    replace_value <- TRUE
  }
  if( alg_value == "et_sf1"){#note that we set sample.fraction to 1 below, as original extratrees uses original sample in each tree
    replace_value <- FALSE
  }
  
  ### Make list of parameters
  parameter_list <- list(
    
    num.trees = list( name = "num.trees",
                      value = ntrees_value ),
    
    min.node.size = list( name = "min.node.size",
                          value = 5),
    
    replace = list( name = "replace",
                    value = replace_value ),
    
    sample.fraction = list( name ="sample.fraction",
                            value = 1),
    
    mtry = list( name = "mtry",
                 value = "to_tune" ),
    
    num.random.splits = list( name ="num.random.splits",
                              value = "to_tune"),
    
    splitrule = list( name = "splitrule",
                      value = "extratrees")
  )
  cat("Created parameter list for ET\n",file=progressfile, append = TRUE)
  
} else {
  stop(paste0("Algorithm ", alg_value, " not supported.") )
}


#set outer dataset
outerfold_ids <- folds_list$outer_folds[[jouter]]
dataset_outer <- dataset_loaded[-outerfold_ids,]
dataset_outer_test <- dataset_loaded[outerfold_ids,]
row.names(dataset_outer) <- NULL
row.names(dataset_outer_test) <- NULL
cat(paste0("Size of outer fold ",jouter,": ",length(outerfold_ids) ), file=progressfile, append = TRUE )

#run inner
res_matrix <- foreach( jinner = icount( nfolds_inner ),  .combine = 'cbind' ) %:%
  foreach(  g_index = icount(grid_size), .combine='c' ) %do% {
    cat("New iteration.\n",
        "Round:          ",round,"\n",
        "Outer fold ID:  ",jouter, "\n",
        "Inner fold ID:  ",jinner, "\n",
        "Param ID (row): ", g_index, "\n\n", file=progressfile, append = TRUE)
    
    ### Inner CV Settings ###
    ### Get inner fold list ###
    ### Attention IDs for the inner folds refer to the IDs for the SUBSET of the dataset, where outer_folds are already REMOVED
    fold_ids <- folds_list$inner_folds_list[[jouter]]$folds
    parameter_list_current <- parameter_list
    nsplits_current <- g_tune[g_index,2] #get num.random.split according to grid index
    mtry_current <-  g_tune[g_index,1] #get mtry according to grid index
    parameter_list_current$num.random.splits$value <- nsplits_current
    parameter_list_current$mtry$value <- mtry_current 
    
    data_cv_train <- dataset_outer[-fold_ids[[jinner]], ]
    data_cv_test  <- dataset_outer[ fold_ids[[jinner]], ]
    row.names( data_cv_train ) <- NULL
    row.names( data_cv_test ) <- NULL
    cat("index in grid: ", g_index, "\n", file=progressfile, append = TRUE)
    cat("nsplits_inner:  ", nsplits_current, "\n", file=progressfile, append = TRUE)
    cat("mtry_current: ",mtry_current, "\n\n", file=progressfile, append = TRUE)
    
    rf <- ranger( formula = formula,
                  data = data_cv_train,
                  num.trees = parameter_list_current$num.trees$value,
                  min.node.size = parameter_list_current$min.node.size$value,
                  replace = parameter_list_current$replace$value,
                  mtry = parameter_list_current$mtry$value,
                  splitrule = parameter_list_current$splitrule$value,
                  sample.fraction = parameter_list_current$sample.fraction$value,
                  num.random.splits = parameter_list_current$num.random.splits$value
    )
    
    predict_rf <- predict(rf, data_cv_test)$predictions
    mse_error <- mean( (predict_rf -  data_cv_test[,response] )^2 )
    cat("Finishing.\n", file=progressfile, append = TRUE)
    return( mse_error )
  }
#end inner loop and cv search 
colnames(res_matrix) <- paste0("fold",seq(1:nfolds_inner))
rownames(res_matrix) <- paste0("g_index.",seq(1:grid_size))



###find best
cv_inner_errors <- rowMeans(res_matrix)
g_index_best <- which.min( cv_inner_errors )#get index of minimum. In unlikely case of two minima, will return the first
#save best setting 
parameter_list_best <- parameter_list
mtry_best <- g_tune[g_index_best,1] #get best mtry value
nsplits_best <- g_tune[g_index_best,2] #get best nsplits value
parameter_list_best$mtry$value <- mtry_best
parameter_list_best$num.random.splits$value <- nsplits_best

saveRDS( 
  list( parameters_best = parameter_list_best,
        round_id = round,
        outer_fold_number = jouter, 
        nfolds_inner = nfolds_inner,
        seed = seed,
        res_matrix = res_matrix ), file = paste0(outputfile_best, "_o",jouter,"_",round,".RDS")
)

#run with best setting
rf_best <- ranger( formula = formula,
                   data = dataset_outer,
                   num.trees = parameter_list_best$num.trees$value,
                   min.node.size = parameter_list_best$min.node.size$value,
                   replace = parameter_list_best$replace$value,
                   mtry = parameter_list_best$mtry$value,
                   splitrule = parameter_list_best$splitrule$value,
                   sample.fraction = parameter_list_current$sample.fraction$value,
                   num.random.splits = parameter_list_best$num.random.splits$value
)

predict_rf_best <- predict(rf_best, dataset_outer_test)$predictions
mse_error_best <- mean( (predict_rf_best -  dataset_outer_test[,response] )^2 )

#save the errors and chosen parameters
mse_error_outer <- mse_error_best
tune_value_chosen <- list( mtry_best = mtry_best, nsplits_best = nsplits_best)

#save error
cat("Results from CV for ET\n\n",
    "Dataset:             ", data_id,"\n",
    "seed:                ", seed, "\n\n",
    "Ranger-Setting.\n",
    "Alg:                 ", alg_value, "\n",
    "nsplits (to tune):   ",nsplits_values_inner,"\n",
    "mtry (to tune):      ",mtry_values_inner,"\n",
    "num_trees:           ",ntrees_value, "\n",
    "replace:             ",replace_value, "\n\n",
    "CV-Setting.\n",
    "no. outer folds:     ", nfolds_outer,"\n",
    "no. inner folds:     ", nfolds_inner,"\n\n",
    "round:               ", round,"\n\n",
    "--------------------\n",
    "----   RESULTS  ----\n",
    "--------------------\n\n",
    "Outer fold id:       ", jouter, "\n",
    "MSE errors:          ", mse_error_outer,"\n", 
    "Chosen parameter per outerfold (in order)\n\n",
    "nsplits:       ", tune_value_chosen$nsplits_best,"\n",
    "mtry values:   ", tune_value_chosen$mtry_best, "\n\n",
    file= paste0(outputfile_all,"_o",jouter,"_",round,".txt")
)

## save parameter list and errors
all_res <- list( data_id = data_id,
                 error = mse_error_outer,
                 outerfold_id = jouter,
                 round_id = round,
                 algorithm = alg_value,
                 tune_value_chosen = tune_value_chosen,
                 seed = seed
)

saveRDS(all_res, file = paste0(outputfile_all,"_o",jouter,"_",round,".RDS") )  
saveRDS(as.data.frame(all_res), file = paste0(outputfile_all,"_o",jouter, "_",round,"_df.RDS") )        

cat("FINISHED ROUND",round,"\n\n", file=progressfile, append = TRUE)



