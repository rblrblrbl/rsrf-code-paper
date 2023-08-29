#Load libraries
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3tuning)

#if getting from console input use below, otherwise set manually in code
#cli <- commandArgs(trailingOnly = TRUE)
#args <- strsplit(cli, "=", fixed = TRUE)
#id_simrun_from_input <- as.numeric( args[[1]] ) #e.g. 118
#isTest_from_input <- as.logical( args[[2]] ) #e.g. "TRUE"
#id_simrun <- id_simrun_from_input
#isTest <- isTest_from_input #Set to false: SERIOUS

#id_simrun <- 149
# isTest <- FALSE

seed_used <- id_simrun #some number or NULL
if(!is.null(seed_used) ){
  set.seed(seed_used)
}


################
### SETTINGS ###

if(isTest){
  #Test
  tasks_dir <- "tasks_test"
  results_dir <- "results_test"
}else{
  #Serious. Make sure the main data files and setup_dt is in that folder
  tasks_dir <- "tasks_serious"
  results_dir <- "results"
}

#Stop if directory is not found.
if(!dir.exists(tasks_dir) ){
  stop(paste("Folder", tasks_dir, "does not exist.") )
}
#Create results directory if not found.
if(!dir.exists(results_dir) ){
  print(paste("Folder", results_dir, "not found.") )
  dir.create(results_dir)
  print(paste("Created folder", results_dir) ) 
}


#PARALLELIZATION setup
#Check function simulation in main_simfile.R for details
#(make sure that future with multicore setting is used on Munir server)
# sim_cores <- 100L
sim_cores <- 1L
#
#CV Setup
if(isTest){
  #TEST
  sim_n_evals <- 1
  sim_n_folds <- 2
}else{
  #SERIOUS
  sim_n_evals <- 200
  sim_n_folds <- 10
}

#Below you can set a range (saved in list sim_sub_index). 
#Then, from list of tasks used, simulations are only conducted for list elements within that range.
#Useful for splitting simulations computation time is long/limited.
#HOW TO USE:
#you can setup the indices lower and upper, if splitting is desired
#Otherwise set sim_sub_index to NULL.
#Attention: Make sure that range supplied is available in task list.
if(isTest){
  #sim_sub_index <- list(lower=1, upper= 1)
  sim_sub_index <- NULL
  lowervals <- seq(1,100,by=2)
  uppervals <- seq(2,100,by=2)
  index_splitjobs <- 1
  sim_sub_index <- list(lower=lowervals[index_splitjobs], upper= uppervals[index_splitjobs])
}else{
  sim_sub_index <- NULL
  # lowervals <- seq(1,100,by=10)
  # uppervals <- seq(10,100,by=10)
  # index_splitjobs <- 1
  # sim_sub_index <- list(lower=lowervals[index_splitjobs], upper= uppervals[index_splitjobs])
}
#Start simulation given specific row entries in setup_dt
#Check up (by hand) the desired simulation IDs by inspecting setup_dt object saved in task_dir
#Modify by hand for each run
##### IMPORTANT ########
if(isTest){
  sim_IDs <- c(136)
}else{
  #SERIOUS
  #sim_IDs <- c(25,26,27) #Ranger CV für A
  sim_IDs <- c(id_simrun) #c( 17 ) #RSRF NF mit CV für A4 (74 für A10, 75 für A30)
}

################
###    RUN   ###
################

source("main_simfile.R")
