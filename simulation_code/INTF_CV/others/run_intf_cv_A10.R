### NOTE for this file: Seeds chosen (second last entry for commandArgs) are wrong in this file due to some error in reporting them.

cv_sim_index <- 1 #Run for 1,2, ... 10. 

if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "10", "A", "10", "1", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("11", "20", "A","10","2", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("21", "30", "A","10","3", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("31", "40", "A","10","4", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("41", "50", "A","10","5", "FALSE")
}
if( cv_sim_index == 6){
  commandArgs <- function(...) c("51", "60", "A", "10", "6", "FALSE")
}
if( cv_sim_index == 7){
  commandArgs <- function(...) c("61", "70", "A","10","7", "FALSE")
}
if( cv_sim_index == 8){
  commandArgs <- function(...) c("71", "80", "A","10","8", "FALSE")
}
if( cv_sim_index == 9){
  commandArgs <- function(...) c("81", "90", "A","10","9", "FALSE")
}
if( cv_sim_index == 10){
  commandArgs <- function(...) c("91", "100", "A","10","10", "FALSE")
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
