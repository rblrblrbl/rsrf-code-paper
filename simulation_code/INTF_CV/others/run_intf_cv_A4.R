cv_sim_index <- 1 #Run for 1,2, ... 5. 

if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "10", "A", "4", "301", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("11", "20", "A","4","302", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("21", "50", "A","4","303", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("51", "75", "A","4","304", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("76", "100", "A","4","305", "FALSE")
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
