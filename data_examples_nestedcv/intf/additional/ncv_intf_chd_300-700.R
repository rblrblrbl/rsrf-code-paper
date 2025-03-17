#File for running Interaction Forests on real dataset using nested CV, for all outer-loop fold-IDs
#with inner k-fold cross-validation
library(doParallel)
library(doRNG)
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
  data_id_from_input <- as.character(args[[1]]) #only chd
  alg_from_input <- as.character( args[[2]])  #intf or intf_norep
  ntrees_from_input <- as.integer( args[[3]] ) #500
  seed_from_input <- as.integer( args[[4]] ) #e.g. 123
  run_with_best_from_input <- as.logical( args[[5]] ) #e.g. TRUE / FALSE
  outer_fold_number_from_input <- as.integer( args[[6]] ) #e.g. 1,2,3,4,5
  round_id_from_input <- as.character( args[[7]] ) #e.g. round1 or round2
  directory <- ""
  outputfile_all    <- paste0( directory, "output/", "nested_cv_", data_id_from_input, "_", alg_from_input,"_j", job_id)
  outputfile_best   <- paste0( directory, "output/", "best_param_", data_id_from_input, "_", alg_from_input,"_j", job_id)
  progressfile <- paste0(submit_dir,"/progress",job_id,".txt")
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

if(alg_value == "intf"){
  replace_value <- TRUE
}
if(alg_value == "intf_norep"){
  replace_value <- FALSE
}
#Get data id
data_id <- data_id_from_input

### Load and set dataset ####
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS")) #dataset
dataset_loaded <- dataset_from_rds$dataset

row.names(dataset_loaded) <- NULL #remove row names
#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
num_covariable <- dataset_from_rds$num_covariable
cat("Loaded dataset: ", data_id, "\n",file=progressfile, append = TRUE)

####
round <- round_id_from_input
if(!(round %in% c("round1","round2") ) ){
  stop("Invalid round id supplied")
}

run_with_best <- run_with_best_from_input
jouter <- outer_fold_number_from_input 

#run for the supplied round value
#Read files
folds_list <- readRDS(paste0("folds/","folds_cv_",data_id,"_",round,".RDS")) #list with fold infos
nfolds_outer <- folds_list$nfolds_outer
nfolds_inner <- folds_list$nfolds_inner

cat("Loaded folds.\n", file=progressfile, append = TRUE)

###
if( alg_value %in% c("intf","intf_norep")){
  ### For looping through npairs values in inner-CV #
  if( data_id %in% c("chd")){
    npairs_values_inner <- seq(300,700,by=100)
    npairs_values_inner_length <- length(npairs_values_inner)
  } else if (data_id %in% c("concrete","airfoil","abalone","robot","abalone_fm", "abalone_enc","concrete_hd","airfoil_hd", "abalone_hd", "robot_hd", "abalone_fm_hd", "abalone_enc_hd")){
    stop("use other file")
  } else if (data_id %in% "chd_hd"){
    stop("use other file")
  } else if (data_id %in% "robot_small"){
    stop("use other file")
  } else{
    stop("Invalid data id.")
  }
  
  cat("Values set up for tuning (INTF), npairs-values to tune: ", npairs_values_inner, "\n",file=progressfile, append = TRUE)
  
  ### Make list of parameters
  parameter_list <- list(
    
    num.trees = list( name = "num.trees",
                      value = ntrees_value ),
    
    npairs = list(name = "npairs",
                  value = "to_tune" ),
    
    min.node.size = list( name = "min.node.size",
                          value = 5),
    
    replace = list( name = "replace",
                    value = replace_value ),
    
    num.threads = list(name = "num.threads", 
                       value = 1),
    
    importance = list(name = "importance", 
                      value = "none")
  )
  cat("Created parameter list for INTF.\n",file=progressfile, append = TRUE)
  
} else {
  stop("algorithm not supported")
}

#set outer dataset
outerfold_ids <- folds_list$outer_folds[[jouter]]
dataset_outer <- dataset_loaded[-outerfold_ids,]
dataset_outer_test <- dataset_loaded[outerfold_ids,]
row.names(dataset_outer) <- NULL
row.names(dataset_outer_test) <- NULL
cat(paste0("Size of outer fold ",jouter,": ",length(outerfold_ids) ), file=progressfile, append = TRUE )

#run inner
### for reproducability
### Following chapter 5.2 of https://cran.r-project.org/web/packages/doRNG/vignettes/doRNG.pdf (work around for nested foreach loops)
### (as of 30/12/2024)
if( onCluster){
  registerDoParallel(cores = procs)
  rng <- RNGseq( npairs_values_inner_length * nfolds_inner + 1, seed)
}
res_matrix <- foreach( jinner = icount( nfolds_inner ),  .combine = 'cbind' ) %:%
  foreach(  m_index = icount(npairs_values_inner_length), r=rng[(jinner-1)*npairs_values_inner_length + 1:npairs_values_inner_length], .combine='c' ) %dopar% {
    cat("New iteration.\n",
        "Outer fold ID: ",jouter, "\n",
        "Inner fold ID: ",jinner, "\n",
        "Param ID: ", m_index, "\n\n", file=progressfile, append = TRUE)
    
    ### For reproducability  # set RNG seed
    rngtools::setRNG(r)
    ### Inner CV Settings ###
    ### Get inner fold list ###
    ### Attention IDs for the inner folds refer to the IDs for the SUBSET of the dataset, where outer_folds are already REMOVED
    fold_ids <- folds_list$inner_folds_list[[jouter]]$folds
    parameter_list_current <- parameter_list
    parameter_list_current$npairs$value <- npairs_values_inner[m_index]
    data_cv_train <- dataset_outer[-fold_ids[[jinner]], ]
    data_cv_test  <- dataset_outer[ fold_ids[[jinner]], ]
    row.names( data_cv_train ) <- NULL
    row.names( data_cv_test ) <- NULL
    
    ### INTF
    intf <- interactionfor( formula = formula,
                            data = data_cv_train,
                            num.trees = parameter_list_current$num.trees$value,
                            min.node.size = parameter_list_current$min.node.size$value,
                            replace = parameter_list_current$replace$value,
                            npairs = parameter_list_current$npairs$value,
                            num.threads = parameter_list_current$num.threads$value,
                            importance = parameter_list_current$importance$value)
    
    
    predict_intf <- predict(intf, data_cv_test)$predictions
    mse_error <- mean( (predict_intf -  data_cv_test[,response] )^2 )
    cat("Finishing.\n", file=progressfile, append = TRUE)
    return( mse_error )
  }

cat("\n\nFinished round: ", round, "\n\n", file=progressfile, append = TRUE)

#end inner loop and cv search 
colnames(res_matrix) <- paste0("fold",seq(1:nfolds_inner))
rownames(res_matrix) <- paste0("m_index.",seq(1:npairs_values_inner_length))



###find best
cv_inner_errors <- rowMeans(res_matrix)
m_index_best <- which.min( cv_inner_errors )#get index of minimum. In unlikely case of two minima, will return the first
#save best setting 
parameter_list_best <- parameter_list
npairs_best <- npairs_values_inner[m_index_best]
parameter_list_best$npairs$value <- npairs_best



#saves best indices
saveRDS( 
  list( parameters_best = parameter_list_best,
        round_id = round,
        outer_fold_number = jouter, 
        nfolds_inner = nfolds_inner,
        seed = seed,
        res_matrix = res_matrix ), file = paste0(outputfile_best, "_o",jouter,"_",round,".RDS")
)


# given the best parameters, run on dataset from outer_cv and evaluate on test set from outer cv
if(!run_with_best){
  cat("Not running with optimal parameters found in inner cv because run_with_best is set to false. You can do this seperately.\n", file = progressfile, append = TRUE )
}
if(run_with_best){
  cat("Running with optimal parameters found in inner cv.\n", file = progressfile, append = TRUE )
}


if( run_with_best){
  #run with best setting
  intf_best <- interactionfor( formula = formula,
                               data = dataset_outer,
                               num.trees = parameter_list_best$num.trees$value,
                               min.node.size = parameter_list_best$min.node.size$value,
                               replace = parameter_list_best$replace$value,
                               npairs = parameter_list_best$npairs$value,
                               num.threads = parameter_list_best$num.threads$value,
                               importance = parameter_list_best$importance$value)
  
  
  predict_intf_best <- predict(intf_best, dataset_outer_test)$predictions
  mse_error_best <- mean( (predict_intf_best -  dataset_outer_test[,response] )^2 )
  
  #save the errors and chosen parameters
  mse_error_outer <- mse_error_best
  npairs_value_chosen <- npairs_best
  
  
  #save mses outer
  saveRDS( list(
    round_id = round,
    outer_fold = jouter,
    mse_error_outer = mse_error_outer,
    npairs_value_chosen = npairs_value_chosen),
    file = paste0("ncv_results_",data_id, "_",alg_value,"_o",jouter,"_",round,".RDS")
  )
  
  #save error
  
  cat("Results from CV for RF\n\n",
      "Dataset:              ", data_id,"\n",
      "seed:                 ", seed, "\n\n",
      "INTF-Setting.\n",
      "Alg:                  ", alg_value, "\n",
      "npairs (to tune):     ",npairs_values_inner,"\n",
      "npairs value (chosen):",npairs_value_chosen, "\n",
      "num_trees:            ",ntrees_value, "\n",
      "replace:              ",replace_value, "\n\n",
      "CV-Setting.\n",
      "no. outer folds:      ", nfolds_outer,"\n",
      "no. inner folds:      ", nfolds_inner,"\n\n",
      "--------------------\n",
      "----   RESULTS  ----\n",
      "--------------------\n\n",
      "Round:                   ", round,"\n",
      "Outer fold:              ", jouter,"\n",
      "MSE error (for this fold)", mse_error_outer,"\n", 
      "npairs values(chosen):   ", npairs_value_chosen, "\n\n",
      file= paste0(outputfile_all,"_","o",jouter,"_",round,".txt")
  )
  
  ## save parameter list and errors
  all_res <- list( data_id = data_id,
                   error = mse_error_outer,
                   round_id = round,
                   algorithm = alg_value,
                   npairs_value_chosen = npairs_value_chosen,
                   seed = seed
  )
  
  saveRDS(all_res, file = paste0(outputfile_all,"_o",jouter,"_",round,".RDS") )   
  saveRDS(as.data.frame(all_res), file = paste0(outputfile_all,"_o",jouter,"_",round,"_df.RDS") )        
  
  
  
}

