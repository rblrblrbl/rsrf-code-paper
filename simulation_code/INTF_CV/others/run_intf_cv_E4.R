cv_sim_index <- 1 #Run for 1,2, ... 5. 

if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "20", "E", "4", "3282", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("21", "40", "E","4","3283", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("41", "60", "E","4","3284", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("61", "80", "E","4","3285", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("81", "100", "E","4","3286", "FALSE")
}


on_cluster <- FALSE
###
if( on_cluster ){
  #get some slurm variables
  
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  job_id <- Sys.getenv("SLURM_JOB_ID")
  
}else{
  procs <- 40
  submit_dir <- getwd()
  job_id <- 123456789
}  
###

source("../intf_cv_server.R")
