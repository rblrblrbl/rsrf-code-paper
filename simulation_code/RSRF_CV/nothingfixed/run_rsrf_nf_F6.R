cv_sim_index <- 1 #Run for 1,2, ... 10. Set to 0 for a test.

if( cv_sim_index == 0){
  commandArgs <- function(...) c("1", "3", "F", "6", "123", "TRUE")
}

if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "10", "F", "6", "65998", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("11", "20", "F","6","65999", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("21", "30", "F","6","66000", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("31", "40", "F","6","66001", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("41", "50", "F","6","66002", "FALSE")
}
if( cv_sim_index == 6){
  commandArgs <- function(...) c("51", "60", "F","6","66003", "FALSE")
}
if( cv_sim_index == 7){
  commandArgs <- function(...) c("61", "70", "F","6","66004", "FALSE")
}
if( cv_sim_index == 8){
  commandArgs <- function(...) c("71", "80", "F","6","66005", "FALSE")
}
if( cv_sim_index == 9){
  commandArgs <- function(...) c("81", "90", "F","6","66006", "FALSE")
}
if( cv_sim_index == 10){
  commandArgs <- function(...) c("91", "100", "F","6","66007", "FALSE")
}

on_cluster <- FALSE
###
if( on_cluster ){
  #get some slurm variables
  
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  job_id <- Sys.getenv("SLURM_JOB_ID")
  
}else{
  procs <- 30
  submit_dir <- getwd()
  job_id <- 123456789
}  
###

source("rsrf_cv_server_nf.R")
