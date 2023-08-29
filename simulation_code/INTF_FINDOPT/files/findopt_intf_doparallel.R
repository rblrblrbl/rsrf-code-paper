library(data.table)
library(doParallel)
library(doRNG)
library(diversityForest)

setDTthreads(1L)
#source util and data generation files
source("util.R")
source("generate_data.R")


cli <- commandArgs(trailingOnly = TRUE)
args <- strsplit(cli, "=", fixed = TRUE)
#e.g. A 4 intf TRUE
type_from_input <- as.character( args[[1]] ) #e.g. "A"
d_from_input <- as.numeric( args[[2]] ) #e.g. "10"
learner_name_from_input <- as.character( args[[3]] ) #e.g. "intf"
isTest_from_input <- as.logical( args[[4]] ) #e.g. "TRUE"
seed_from_input <- as.numeric( args[[5]] )

### Use below if you want to set input in this file.
# type_from_input <- "A" #e.g. "A"
# d_from_input <- 4
# learner_name_from_input <- "intf" #e.g. "intf"
# isTest_from_input <- TRUE #e.g. "FALSE"

type <- type_from_input
d <- d_from_input
n <- 500
learner_name <- learner_name_from_input

if( isTest_from_input == TRUE){
  random_design_size <- 3
  nsims <- 2
}else{
  random_design_size <- 200
  nsims <- 30
}

#do parallel
#get some slurm variables
# procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
# submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
# job_id <- Sys.getenv("SLURM_JOB_ID")
#windows
# setwd("~/Universitaet-Projekte/Projekt-Paper-Chi-et-al/R-Code/rsrfdev/simulate_large/cluster_bw/intf_findopt_doparallel/new")
# procs <- 1
# submit_dir <- "C:/Users/ricar/Documents/Universitaet-Projekte/Projekt-Paper-Chi-et-al/R-Code/rsrfdev/simulate_large/cluster_bw/intf_findopt_doparallel/new"
# job_id <- "windows123456789"

#setup progress file in submit directory
name_fileoutput <- paste0( type, d, "_", learner_name )
progress_filename <- paste0( submit_dir, "/progress_", name_fileoutput, "_job_id_", job_id, ".txt")
cat( "Submit-Dir: ", submit_dir, "\n",  file = progress_filename, append =FALSE)
cat( "Job-ID: ", job_id, "\n", file=progress_filename, append=TRUE)

#output
output_folder <- "results_find_opt"
output_filename <- paste0(  output_folder, "/", name_fileoutput, ".txt")

#print them
print(paste("Procs", procs))
print(paste("Submit Directory", submit_dir))
print(paste("Job ID", job_id))

#parallel server setting
registerDoParallel(cores=procs)
registerDoRNG( seed = seed_from_input, once = TRUE)
mcoptions <- list(preschedule=FALSE)


cat(file=output_filename,
    "Results Optimal parameter search.\n\n",
    "Type:                    ", type, "\n",
    "n (sample/test size)     ", n, "\n",
    "d (Dimension)            ", d, "\n",
    "nsims                    ", nsims, "\n",
    "Learner                  ", learner_name, "\n\n",
    "Random Design Size       ", random_design_size, "\n\n",
    "Random seed used (doRNG) ", seed_from_input, "\n\n", 
    "------------------------------------\n\n")


if( d== 4) npairs_range <- 100
if( d== 6) npairs_range <- 150
if( d== 10) npairs_range <- 250
if( d== 30) npairs_range <- 750

##set tuning parameters
if( isTest_from_input == TRUE){
  parameter_list_not_tuned <- list(
    #fixed parameters
    num.threads = list(name = "num.threads", 
                       value = 1, 
                       tune = FALSE, 
                       tune_vals = NA),
    
    importance = list(name = "importance", 
                      value = "none", 
                      tune = FALSE, 
                      tune_vals = NA),
    
    num.trees = list(name = "num.trees", 
                     value = 100, 
                     tune = FALSE, 
                     tune_vals = NA),
    
    #flexible (tuning) parameters
    npairs = list(name = "npairs",
                  value = NA,
                  tune = TRUE,
                  tune_vals = 151:152 ),
    
    min.node.size = list(name = "min.node.size",
                         value = NA,
                         tune = TRUE,
                         tune_vals = 5:6),
    
    replace = list(name = "replace",
                   value = TRUE,
                   tune =FALSE,
                   tune_vals = NA )
  )
}else{
  parameter_list_not_tuned <- list(
    #fixed parameters
    num.threads = list(name = "num.threads", 
                       value = 1, 
                       tune = FALSE, 
                       tune_vals = NA),
    
    importance = list(name = "importance", 
                      value = "none", 
                      tune = FALSE, 
                      tune_vals = NA),
    
    num.trees = list(name = "num.trees", 
                     value = 500, 
                     tune = FALSE, 
                     tune_vals = NA),
    
    #flexible (tuning) parameters
    npairs = list(name = "npairs",
                  value = NA,
                  tune = TRUE,
                  tune_vals = 1:npairs_range ),
    
    min.node.size = list(name = "min.node.size",
                         value = NA,
                         tune = TRUE,
                         tune_vals = 5:30),
    
    replace = list(name = "replace",
                   value = NA,
                   tune =TRUE,
                   tune_vals = c(TRUE,FALSE) )
  )
}  

#Generate Random Design
param_design <- data.frame( matrix(NA, nrow= random_design_size, ncol=0 ))
for( k in 1:length(parameter_list_not_tuned) ){
  param <- parameter_list_not_tuned[[k]]
  if( isTRUE( param$tune ) ){
    param_design[ param$name ] <- param$tune_vals[sample.int( length(param$tune_vals), replace = TRUE, size = random_design_size)]
    print(paste0( "Sampled design points for parameter:", param$name) )
  }else{
    param_design[ param$name ] <- rep( param$value, times = random_design_size)
  }
}
print("Created design.")
print( head( param_design) )
param_design_result <- param_design
param_design_result["design_id"] <- 1:random_design_size
##

start_time <- Sys.time()
#simulations
result <- foreach( l = icount(random_design_size) ) %dopar% {
  print(paste0("Start for design index :", l) )
  cat( "Start for design index :", l,  "\n", file = progress_filename, append = TRUE )
  #get current parameter list
  param_list_current <- param_design[l,]
  ptime <- system.time(
    {
      error_list <- foreach( i = icount(nsims) ) %do% {
        #generate train and test data
        if( type == "A"){
          m_reg <- m_pure_sparse
          regdata_train<- subset( generateDataNisp( n = n, d = d, m_reg = m_reg), select = -mx)
          regdata_test <- subset( generateDataNisp( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
        }
        if( type == "B"){
          m_reg <- m_hierarchical
          regdata_train<- subset( generateDataNisp( n = n, d = d, m_reg = m_reg), select = -mx)
          regdata_test <- subset( generateDataNisp( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
        }
        if( type == "C"){
          m_reg <- m_additive
          regdata_train<- subset( generateDataNisp( n = n, d = d, m_reg = m_reg), select = -mx)
          regdata_test <- subset( generateDataNisp( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
        }
        if( type == "D"){
          m_reg <- function(x1,x2,x3, ...) return( m_interact2_factor( factor = 3, x1,x2,x3))
          regdata_train<- subset( generateDataUnif( n = n, d = d, m_reg = m_reg), select = -mx)
          regdata_test <- subset( generateDataUnif( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
        }
        if( type == "E"){
          m_reg <- function(x1,x2,x3, ...) return( m_interact2_factor( factor = 5, x1,x2,x3))
          regdata_train<- subset( generateDataUnif( n = n, d = d, m_reg = m_reg), select = -mx)
          regdata_test <- subset( generateDataUnif( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
        }
        
        if( type == "F"){
          m_reg <- function(x1,x2,x3,x4,x5,x6, ...) return( m_interact2_factor_add(x1,x2,x3,x4,x5,x6,A=10,B=1))
          regdata_train<- subset( generateDataUnif( n = n, d = d, m_reg = m_reg), select = -mx)
          regdata_test <- subset( generateDataUnif( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
        }
        
        #run learner
        
        print(paste0("Trial: ", i) )
        intf <- interactionfor( y ~ .,
                                data = regdata_train,
                                num.threads = param_list_current$num.threads,
                                num.trees = param_list_current$num.trees,
                                min.node.size = param_list_current$min.node.size,
                                replace = param_list_current$replace,
                                importance = param_list_current$importance,
                                npairs = param_list_current$npairs
        )
        predict_intf <- predict( intf, regdata_test )$predictions
        error <- mean( (predict_intf - regdata_test$y )^2 )
        return( error )
      }
    }
  )[3]
  cat( "Time taken:", ptime,  "\n", file = progress_filename, append = TRUE )
  return( list( design_index = l,
                error_list = error_list,
                error = mean( sapply( 1:length(error_list), function(r) return( error_list[[r]]) )  ) ) )
}
end_time <- Sys.time()


#Obtain best parameters by evaluating choosing smallest mean of regr.mse (among ID's) 
design_ids <- 1:random_design_size
#get error as vector
errors <- sapply( result , function(x) return(x$error) )

param_design_result <- setDT( param_design_result )
param_design_result[,error := errors]

#Get best ID and sort param_design_result
setorder( param_design_result , error )
bestID <- param_design_result$design_id[1]
best_mse <- param_design_result$error[1]


#tune-parameters
all_names <- sapply( parameter_list_not_tuned, function(x) x$name )
where_tune <- sapply( parameter_list_not_tuned, function(x) x$tune )
parameters_names_tune <- all_names[where_tune]
#fix-parameters
parameters_names_fixed <- all_names[!where_tune]

txt_feedback <- "Found optimal parameters."



cat(file=output_filename,
    "--------------------------\n\n",
    txt_feedback, "\n",
    "Runtime", format( difftime( end_time, start_time) ), "\n",
    "--------------------------\n",
    "Parameters: \n\n", append=TRUE)


cat(file=output_filename, "\n",
    "--------------------------\n\n",
    "Fixed Parameters: \n\n", append=TRUE)

for( name in parameters_names_fixed){
  cat( file=output_filename, 
       name, "____________", toString( param_design_result[1,][,..name] ),"\n\n", append=TRUE)
}


cat(file=output_filename, "\n",
    "--------------------------\n\n",
    "Tuned (optimal) Parameters: \n\n", append=TRUE)

for( name in parameters_names_tune){
  cat( file=output_filename, 
       name, "____________", toString( param_design_result[1,][,..name] ),"\n\n", append=TRUE)
}
cat(file=output_filename, "--------------------------\n\n", "ID: ", bestID, "\n", "mean mse:", best_mse, "\n", append = TRUE)

cat(file=output_filename, "\n",
    "--------------------------\n\n",
    "Different style: \n\n", append=TRUE)


write.table(param_design_result[1,..parameters_names_tune], sep = "********", append=TRUE, file=output_filename, row.names = FALSE)

#saveRDS
result_save <- list( result, param_design_result)
saveRDS( result_save, file = paste0(output_filename, ".RDS") )
