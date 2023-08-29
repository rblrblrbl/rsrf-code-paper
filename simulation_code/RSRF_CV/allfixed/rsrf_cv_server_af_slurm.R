library(doParallel)
library(doRNG)
library(simpleRSRF)
# Note: If "cores=..." is omitted in the statement below, the number of cores
# is set to one-half the number of cores detected by the parallel package.
# CV for simpleRSRF using doParallel
# note: not working on windows

procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
job_id <- Sys.getenv("SLURM_JOB_ID")

#print them
print(paste("Procs", procs))
print(paste("Submit Directory", submit_dir))
print(paste("Job ID", job_id))

#setup progress file in submit directory
progress_filename <- paste0( submit_dir, "/progress_", job_id, ".txt")
cat( "Submit-Dir: ", submit_dir, "\n",  file = progress_filename, append =FALSE)
cat( "Job-ID: ", job_id, "\n", file=progress_filename, append=TRUE)



#e.g. R CMD BATCH --no-save --no-restore '--args 5 7 A 4' t.R
args=(commandArgs(TRUE))
lower_from_input <- as.integer(args[[1]])
upper_from_input <- as.integer(args[[2]])
print(lower_from_input)
print(upper_from_input)

model_index <- as.character(args[[3]])
d <- as.integer(args[[4]])


seed_from_input <- as.numeric( args[[5]] )

isTest <- as.logical( args[[6]] )

#setup parallel
registerDoParallel(cores=procs)
mcoptions <- list(preschedule=FALSE)
#possibly set seed using DoRNG
if( !is.na(seed_from_input) ){
  registerDoRNG( seed = seed_from_input, once = TRUE)
}


print( paste("Model index:", model_index))
print( paste("Dimension d:", d))
print( paste("Is test?", isTest))
print( paste("Seed", seed_from_input))

#write to progress
cat( "Model index:", model_index, "\n",  
     "Dimension d:", d, "\n", 
     "Is test?", isTest, "\n", 
     "seed chosen", seed_from_input,"\n",
     file=progress_filename, append=TRUE)


t_dir <- "tasks_serious"

filename_train <- paste0( t_dir, "/", model_index, toString(d), "_train_list.RDS")
if( !file.exists( filename_train ) ){
  stop("Task file missing")
}

filename_test <- paste0( t_dir, "/", model_index, toString(d), "_test_list.RDS")
if( !file.exists( filename_test ) ){
  stop("Task file missing")
}

if(isTest){
    nfolds <- 5 #between 1 and nsims
    trials <- 8 #serious: 200
}else{
    nfolds <- 10
    trials <- 200
}

cat( "nfolds: ", nfolds, "\n", "trials: ", trials, "\n",
     "Data index range: ", "\n",
     "...lower: ", lower_from_input, "\n",
     "...upper: ", upper_from_input, "\n\n", file=progress_filename, append=TRUE)


#load
train_list <- readRDS(filename_train)
test_list <- readRDS(filename_test)

#output name
out_name <- paste0(model_index, toString(d), "_rsrf_cv" ) 

#set tuning parameters

#set tuning parameters
parameter_list_not_tuned <- list(
  #fixed parameters
  mtry_mode = list(name = "mtry_mode", 
                   value = "allfixed", 
                   tune = FALSE, 
                   tune_vals = NA),
  
  fixed_cart_cart = list(
    name = "fixed_cart_cart",
    value = TRUE,
    tune = FALSE,
    tune_vals = NA),
  
  rsrf_depth = list(name = "rsrf_depth",
                    value = 2,
                    tune = FALSE,
                    tune_vals = NA),
  
  mtry_rsrf_step_random = list(name = "mtry_rsrf_step_random",
                               value = NULL,
                               tune = TRUE,
                               tune_vals = 1:d),
  
  num_trees = list(name = "num_trees",
                   value = 100,
                   tune = FALSE,
                   tune_vals = NA),
  
  #flexible (tuning) parameters
  rsrf_width = list(name = "rsrf_width",
                    value = NA,
                    tune = TRUE,
                    tune_vals = 1:ifelse(d == 30, 30, 15) ),
  
  mtry_rsrf_step = list(name = "mtry_rsrf_step",
                        value = NA,
                        tune = TRUE,
                        tune_vals = 1:d ),
  
  mtry_cart_cart = list(name = "mtry_cart_cart", 
                        value = NULL,
                        tune = FALSE,
                        tune_vals = NA ),
  
  min_node_size = list(name = "min_node_size",
                       value = NA,
                       tune = TRUE,
                       tune_vals = 5:30),
  
  replace = list(name = "replace",
                 value = NA,
                 tune =TRUE,
                 tune_vals = c(TRUE,FALSE) ),
  
  include_CART_CART = list(name = "include_CART_CART",
                           value= NA,
                           tune =TRUE,
                           tune_vals = c(TRUE,FALSE) )
)

cat( "--- STARTING SIMULATION --- \n\n", file=progress_filename, append=TRUE)

#simulations
result <- foreach( data_index = lower_from_input:upper_from_input) %do% {
    print(paste0("Start for data_index :", data_index) )
    cat( "Start for data_index :", data_index,  "\n", file = progress_filename, append = TRUE ) 
    regdata_train <- train_list[[data_index]]
    
    if(isTest){ 
      test_nsample <- 50
      regdata_train <- regdata_train[1:test_nsample,] 
      }
    
    nsample <- nrow(regdata_train)
    
    ptime <- system.time({
    r <- foreach(i=icount(trials), .options.multicore=mcoptions ) %dopar% {
        print(paste0("Trial: ", i) )
        #initialize current parameter list
        param_list_current <- parameter_list_not_tuned
        #updata current parameter list by sampling some tuning instance
        
        
        for( k in 1:length(param_list_current) ){
          param <- param_list_current[[k]]
          if( isTRUE( param$tune ) ){
            param_list_current[[k]]$value <- param$tune_vals[sample.int( length(param$tune_vals), size = 1)]
            print(paste0( param_list_current[[k]]$name, ": ", param_list_current[[k]]$value) )
          } 
        }
        #create equal sized cv blocks (requires that nsample can be divided by nfolds)
        neworder <- sample(1:nsample)
        a <- seq(1,nsample, by = nsample/nfolds)
        b <- seq(nsample/nfolds ,nsample, by = nsample/nfolds)
        fold_ids <- lapply( 1:nfolds, function(i) neworder[seq(a[i],b[i]) ] )
        
        # cv loop
        cv_errors <- foreach(j = icount(nfolds), .combine = c ) %do% {
          regdata_cv_train <- regdata_train[ -fold_ids[[j]], ]
          regdata_cv_test  <- regdata_train[fold_ids[[j]], ]
          rsrf <- simpleRSRF( y ~ .,
                              data = regdata_cv_train,
                              num_threads = 1,
                              randomization = TRUE,
                              saveNodeInformation = FALSE,
                              num_trees = param_list_current$num_trees$value,
                              min_node_size = param_list_current$min_node_size$value,
                              replace = param_list_current$replace$value,
                              rsrf_width = param_list_current$rsrf_width$value,
                              mtry_mode = param_list_current$mtry_mode$value,
                              mtry_rsrf_step_random = param_list_current$mtry_rsrf_step_random$value,
                              mtry_rsrf_step = param_list_current$mtry_rsrf_step$value,
                              mtry_cart_cart = param_list_current$mtry_cart_cart$value,
                              fixed_cart_cart = param_list_current$fixed_cart_cart$value,
                              rsrf_depth = param_list_current$rsrf_depth$value,
                              include_CART_CART = param_list_current$include_CART_CART$value
          )
          predict_rsrf <- rsrf$predict( regdata_cv_test )
          return( mean( (predict_rsrf - regdata_cv_test$y )^2 ) )
        }
        #calculate mean error over cv iterations
        cv_error <- mean( cv_errors )
        return( list(parameter_list = param_list_current, cv_error = cv_error ) )
    }})[3]

    cat(sprintf('Parallel time using doParallel on %d workers: %f\n', getDoParWorkers(), ptime))
    cat(sprintf('Parallel time using doParallel on %d workers: %f\n', getDoParWorkers(), ptime),  "\n",
                 "   ... in hours:", ptime/3600, "\n", file = progress_filename, append = TRUE ) 
    
    
    ## extract best
    cv_all_errors <- sapply( 1:trials, function(i){ return( r[[i]]$cv_error ) })
    best_index <- which.min( cv_all_errors )
    param_list_cv_opt <- r[[best_index]]$parameter_list
    
    print("Extracted best")

    # evaluate with best parameters
    rsrf_opt <- simpleRSRF( y ~ .,
                        data = regdata_train,
                        num_threads = 1,
                        randomization = TRUE,
                        saveNodeInformation = FALSE,
                        num_trees = param_list_cv_opt$num_trees$value,
                        min_node_size = param_list_cv_opt$min_node_size$value,
                        replace = param_list_cv_opt$replace$value,
                        rsrf_width = param_list_cv_opt$rsrf_width$value,
                        mtry_mode = param_list_cv_opt$mtry_mode$value,
                        mtry_rsrf_step_random = param_list_cv_opt$mtry_rsrf_step_random$value,
                        mtry_rsrf_step = param_list_cv_opt$mtry_rsrf_step$value,
                        mtry_cart_cart = param_list_cv_opt$mtry_cart_cart$value,
                        fixed_cart_cart = param_list_cv_opt$fixed_cart_cart$value,
                        rsrf_depth = param_list_cv_opt$rsrf_depth$value,
                        include_CART_CART = param_list_cv_opt$include_CART_CART$value
    )
    
    

    regdata_test <- test_list[[data_index]]
    if(isTest){ 
      regdata_test <- regdata_test[1:test_nsample,] 
    }
    predict_rsrf_test <- rsrf_opt$predict( regdata_test )
    error <- mean( (predict_rsrf_test - regdata_test$y )^2 )
    cat( "Finished simulation (CV).\n\n", "Data index:", data_index, "\n", "Error: ", error, "\n\n") 
    cat( "Finished simulation (CV).\n\n", "Data index:", data_index, "\n", "Error: ", error, "\n\n", file=progress_filename, append = TRUE) 
    result <- list( data_index = data_index, error = error, param_list_cv = param_list_cv_opt )
   
  
}

    saveRDS(result, paste0("results/result_", out_name,"_", lower_from_input, "_", upper_from_input, ".RDS") )