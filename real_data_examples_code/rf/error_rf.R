#File for running RF on real dataset with k-fold cross-validation
library(foreach)
library(iterators)
onCluster <- FALSE
if(!onCluster){
  mydirectory <- getwd()
  setwd(mydirectory)
  if(!dir.exists("output_cv_local")){
    dir.create("output_cv_local")
  }
  args=(commandArgs(TRUE))
  if( length(args) > 0 ){
    data_id_from_input <- as.character(args[[1]]) #e.g. airfoil
    alg_from_input <- as.character(args[[2]]) # put "rf" here
    is_test_from_input <- as.logical( args[[3]] ) #FALSE
    mtry_from_input <- as.integer( args[[4]] ) #e.g. 3
    ntrees_from_input <- as.integer( args[[5]] ) #500
    seed_from_input <- as.integer( args[[6]] ) #e.g. 9
    nfolds_from_input <- as.integer( args[[7]] ) #e.g. 10
  }else{
    mtry_from_input <- 5
    alg_from_input <- "rf"
    data_id_from_input <- "airfoil"
    is_test_from_input <- FALSE
    seed_from_input <- 9
    nfolds_from_input <- 10
    ntrees_from_input <- 500
  }
  
  seed <- seed_from_input
  set.seed(seed)
  submit_dir <- getwd()
  
  job_id <- "local"
  directory <- mydirectory
  outputfile_all_in_one   <- paste0( directory, "/output_cv_local/", ifelse( is_test_from_input, "test_",""), "CV_all_", data_id_from_input, "_", alg_from_input, "_mtry_",mtry_from_input, "_ntrees_", ntrees_from_input)
  outputfile_fold_information   <- paste0( directory, "/output_cv_local/fold_info_cv", ifelse( is_test_from_input, "test_",""), "CV_all_", data_id_from_input, "_", alg_from_input)
}


### Setting of dataset ####
data_id <- data_id_from_input
dataset_from_rds <- readRDS(paste0( "datasets/dataset", "_", data_id, ".RDS"))
dataset <- dataset_from_rds$dataset
row.names(dataset) <- NULL #remove row names

#test
isTest <- is_test_from_input
if( isTest ){
  stop("Test mode not available.")
}

#Get response etc., set formula
response <- dataset_from_rds$response
formula <- as.formula( paste0( response, "~ ." ))
n_data <- nrow( dataset )
num_covariables <- dataset_from_rds$num_covariables

### CV Settings ###
nfolds <- nfolds_from_input
alg_value <- alg_from_input
mtry_value <- mtry_from_input
ntrees_value <- ntrees_from_input

if( alg_value == "rf" ){
### Make list of parameters
parameter_list <- list(
  mtry = list(name = "mtry", 
                   value = mtry_value),
  
  num.trees = list(name = "num.trees",
                   value = ntrees_value),
  
  min.node.size = list(name = "min.node.size",
                       value = 5),
  
  replace = list(name = "replace",
                 value = TRUE)
)
}

#### CV CODE ####

### Create Folds ###
neworder <- sample(1:n_data)
a <- round( seq(1,n_data, by = n_data/nfolds) )
b <- round( seq(n_data/nfolds ,n_data, by = n_data/nfolds) )
fold_ids <- lapply( 1:nfolds, function(i) neworder[seq(a[i],b[i]) ] )
for( j in 1:length(fold_ids)){
  print(paste0("Size of group ",j,": ",length(fold_ids[[j]])))
}

cv_results <- foreach(j = icount(nfolds) ) %do% {
    data_cv_train <- dataset[-fold_ids[[j]], ]
    data_cv_test  <- dataset[ fold_ids[[j]], ]
    rf <- ranger::ranger( formula = formula,
                        data = data_cv_train,
                        num.trees = parameter_list$num.trees$value,
                        min.node.size = parameter_list$min.node.size$value,
                        replace = parameter_list$replace$value,
                        mtry = parameter_list$mtry$value
    )
    
    predict_rf <- predict(rf, data_cv_test)$predictions
    mse_error <- mean( (predict_rf -  data_cv_test[,response] )^2 )
    return( list(cv_error = mse_error ) )
}

## extract
cv_all_errors <- sapply( 1:nfolds, function(i){ return( cv_results[[i]]$cv_error ) })
cv_mean_error <- mean(cv_all_errors)
sd_error <- sd(cv_all_errors)

cat("Results from CV for RANDOM FORESTS\n\n",
    "Dataset:      ", data_id,"\n",
    "seed:         ", seed, "\n\n",
    "algoritm:     ", alg_value, "\n\n",
    "RANDOM FOREST-Setting.\n",
    "mtry:         ", mtry_value, "\n",
    "ntrees:       ", ntrees_value, "\n",
    "CV-Setting.\n",
    "no. folds", nfolds,"\n\n",
    "--------------------\n",
    "----   RESULTS  ----\n",
    "--------------------\n\n",
    "est. MSE:     ", cv_mean_error,"\n",
    "standard dev: ", sd_error,"\n\n\n",
    file= paste0(outputfile_all_in_one,".txt")
)

## save parameter list and errors
all_res <- list( data_id = data_id,
                 nfolds = nfolds,
                 cv_errors = cv_all_errors,
                 mean_error = cv_mean_error,
                 sd = sd_error,
                 algorithm = alg_value,
                 mtry = mtry_value,
                 ntrees = ntrees_value,
                 parameters = parameter_list,
                 seed = seed,
                 folds = fold_ids )

saveRDS(all_res, file = paste0(outputfile_all_in_one,".RDS") )              
