#File for running RF on real dataset using nested CV, for all outer-loop fold-IDs
#with inner k-fold cross-validation
library(ranger)
library(foreach)
library(iterators)

onCluster <- TRUE
if(!dir.exists("output")){
  dir.create("output")
}

if(onCluster){
  args=(commandArgs(TRUE))
  job_id <- Sys.getenv("SLURM_JOB_ID")
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  data_id_from_input <- as.character(args[[1]]) #e.g. robot / robot_small
  alg_from_input <- as.character( args[[2]])  #rf #et #rf_norep
  ntrees_from_input <- as.integer( args[[3]] ) #500
  seed_from_input <- as.integer( args[[4]] ) #e.g. 123, or "NA"
  round_from_input <- as.character( args[[5]]) #either "both" or "round1" or "round2"
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
  #setwd( ... ) set to file where code is so
  data_id_from_input <- "airfoil" #e.g. airfoil
  alg_from_input <- "rf"  #rf #et
  ntrees_from_input <- 500 #500
  replace_from_input <- TRUE
  seed_from_input <- 3  #e.g. 123, or "NA"
  round_from_input <- "both"
  seed <- seed_from_input
  set.seed(seed)
  submit_dir <- getwd()
  job_id <- "local"
  directory <- getwd()
  outputfile_all    <- paste0( directory, "/output/", "nested_cv_", data_id_from_input, "_", alg_from_input)
  outputfile_best   <- paste0( directory, "/output/", "best_param_", data_id_from_input, "_", alg_from_input)
  progressfile <- ""
}

### values from input
alg_value <- alg_from_input
ntrees_value <- ntrees_from_input

if(alg_value == "rf"){
  replace_value <- TRUE
}
if(alg_value == "rf_norep"){
  replace_value <- FALSE
}
#Get data id
data_id <- data_id_from_input

#decide which of the round (or both) shall be ran
if( !( round_from_input %in% c("both","round1","round2") ) ){
  stop("Invalid round-value supplied.")
} else{
  if( round_from_input == "both"){
    round_vals <- c("round1", "round2")
  } else{
    round_vals <- round_from_input
  }
}

### Load and set dataset ####
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS")) #dataset
dataset_loaded <- dataset_from_rds$dataset

row.names(dataset_loaded) <- NULL #remove row names
#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
num_covariable <- dataset_from_rds$num_covariable
cat("Loaded dataset: ", data_id, "\n",file=progressfile, append = TRUE)

#run for each of the round_vals
for( round in round_vals){
  #Read files
  folds_list <- readRDS(paste0("folds/","folds_cv_",data_id,"_",round,".RDS")) #list with fold infos
  nfolds_outer <- folds_list$nfolds_outer
  nfolds_inner <- folds_list$nfolds_inner
  
  cat("Loaded folds.\n", file=progressfile, append = TRUE)
  
  ###
  
  if( alg_value %in% c("rf","rf_norep")){
    ### For looping through mtry values in inner-CV #
    if( data_id %in% c("concrete","airfoil","robot","chd","abalone_enc")){
      mtry_values_inner <- 1:num_covariable
      mtry_values_inner_length <- length(mtry_values_inner)
    } else if (data_id %in% c("concrete_hd","airfoil_hd","robot_hd", "chd_hd", "abalone_enc_hd")){
      mtry_values_inner <- seq(5,num_covariable,by=5)
      mtry_values_inner_length <- length(mtry_values_inner)
    } else if (data_id %in% "robot_small"){
      mtry_values_inner <- c(2,3,4,5)
      mtry_values_inner_length <- length(mtry_values_inner)
    } else{
      stop("Invalid data id.")
    }
    
    cat("Values set up for tuning (RF), mtry-values to tune: ", mtry_values_inner, "\n",file=progressfile, append = TRUE)
  
    ### Make list of parameters
    parameter_list <- list(
      
      num.trees = list( name = "num.trees",
                        value = ntrees_value ),
      
      min.node.size = list( name = "min.node.size",
                            value = 5),
      
      replace = list( name = "replace",
                      value = replace_value ),
      
      mtry = list( name = "mtry",
                   value = "to_tune" )
    )
    cat("Created parameter list for RF.\n",file=progressfile, append = TRUE)
    
  } else {
    stop("algorithm not supported")
  }
  
  mse_errors_outer <- numeric(0) #saves mse error from outer test fold
  mtry_values_chosen <- numeric(0) #saves chosen tuning parameter value per outer fold
  
  for( jouter in 1:nfolds_outer){
    #set outer dataset
    outerfold_ids <- folds_list$outer_folds[[jouter]]
    dataset_outer <- dataset_loaded[-outerfold_ids,]
    dataset_outer_test <- dataset_loaded[outerfold_ids,]
    row.names(dataset_outer) <- NULL
    row.names(dataset_outer_test) <- NULL
    cat(paste0("Size of outer fold ",jouter,": ",length(outerfold_ids) ), file=progressfile, append = TRUE )
    
    #run inner
    res_matrix <- foreach( jinner = icount( nfolds_inner ),  .combine = 'cbind' ) %:%
       foreach(  m_index = icount(mtry_values_inner_length), .combine='c' ) %do% {
        cat("New iteration.\n",
            "Outer fold ID: ",jouter, "\n",
            "Inner fold ID: ",jinner, "\n",
            "Param ID: ", m_index, "\n\n", file=progressfile, append = TRUE)
  
        ### Inner CV Settings ###
        ### Get inner fold list ###
        ### Attention IDs for the inner folds refer to the IDs for the SUBSET of the dataset, where outer_folds are already REMOVED
        fold_ids <- folds_list$inner_folds_list[[jouter]]$folds
        parameter_list_current <- parameter_list
        parameter_list_current$mtry$value <- mtry_values_inner[m_index]
        data_cv_train <- dataset_outer[-fold_ids[[jinner]], ]
        data_cv_test  <- dataset_outer[ fold_ids[[jinner]], ]
        row.names( data_cv_train ) <- NULL
        row.names( data_cv_test ) <- NULL
      
        ###
        rf <- ranger::ranger( formula = formula,
                              data = data_cv_train,
                              num.trees = parameter_list_current$num.trees$value,
                              min.node.size = parameter_list_current$min.node.size$value,
                              replace = parameter_list_current$replace$value,
                              mtry = parameter_list_current$mtry$value
        )
      
        predict_rf <- predict(rf, data_cv_test)$predictions
        mse_error <- mean( (predict_rf -  data_cv_test[,response] )^2 )
        cat("Finishing.\n", file=progressfile, append = TRUE)
        return( mse_error )
       }
      #end inner loop and cv search 
      colnames(res_matrix) <- paste0("fold",seq(1:nfolds_inner))
      rownames(res_matrix) <- paste0("m_index.",seq(1:mtry_values_inner_length))
      
      
      
      ###find best
      cv_inner_errors <- rowMeans(res_matrix)
      m_index_best <- which.min( cv_inner_errors )#get index of minimum. In unlikely case of two minima, will return the first
      #save best setting 
      parameter_list_best <- parameter_list
      mtry_best <- mtry_values_inner[m_index_best]
      parameter_list_best$mtry$value <- mtry_best
      
      saveRDS( 
        list( parameters_best = parameter_list_best,
              round_id = round,
              outer_fold_number = jouter, 
              nfolds_inner = nfolds_inner,
              seed = seed,
              res_matrix = res_matrix ), file = paste0(outputfile_best, "_o",jouter,"_",round,".RDS")
      )
      
      #run with best setting
      rf_best <- ranger::ranger( formula = formula,
                            data = dataset_outer,
                            num.trees = parameter_list_best$num.trees$value,
                            min.node.size = parameter_list_best$min.node.size$value,
                            replace = parameter_list_best$replace$value,
                            mtry = parameter_list_best$mtry$value
      )
      
      predict_rf_best <- predict(rf_best, dataset_outer_test)$predictions
      mse_error_best <- mean( (predict_rf_best -  dataset_outer_test[,response] )^2 )
      
      #save the errors and chosen parameters
      mse_errors_outer[jouter] <- mse_error_best
      mtry_values_chosen[jouter] <- mtry_best
      
  }
  
  saveRDS( list(
    round_id = round,
    mse_errors_outer = mse_errors_outer,
    mtry_values_chosen = mtry_values_chosen),
    file = paste0("ncv_results_",data_id, "_",alg_value,"_",round,".RDS")
  )
  
    cat("Results from CV for RF\n\n",
        "Dataset:             ", data_id,"\n",
        "seed:                ", seed, "\n\n",
        "Ranger-Setting.\n",
        "Alg:                 ", alg_value, "\n",
        "mtry (to tune):      ",mtry_values_inner,"\n",
        "mtry values (chosen):",mtry_values_chosen, "\n",
        "num_trees:           ",ntrees_value, "\n",
        "replace:             ",replace_value, "\n\n",
        "CV-Setting.\n",
        "no. outer folds:     ", nfolds_outer,"\n",
        "no. inner folds:     ", nfolds_inner,"\n\n",
        "--------------------\n",
        "----   RESULTS  ----\n",
        "--------------------\n\n",
        "MSE errors:          ", mse_errors_outer,"\n", 
        "mtry values (chosen):", mtry_values_chosen, "\n\n",
        "mean of MSEs:        ", mean(mse_errors_outer),"\n\n",
        file= paste0(outputfile_all,"_",round,".txt")
    )
    
    ## save parameter list and errors
    all_res <- list( data_id = data_id,
                     errors = mse_errors_outer,
                     round_id = round,
                     mean_error = mean(mse_errors_outer),
                     algorithm = alg_value,
                     mtry_values_chosen = mtry_values_chosen,
                     seed = seed
    )
    
    saveRDS(all_res, file = paste0(outputfile_all,"_",round,".RDS") )   
    saveRDS(as.data.frame(all_res), file = paste0(outputfile_all,"_",round,"_df.RDS") )        
    
    cat("\n\nFinished round: ", round, file=progressfile, append = TRUE)
    
}



