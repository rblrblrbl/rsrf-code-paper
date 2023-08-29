# Note: Results for this file (find opt interaction forests B30) will be different than the ones in paper due to missing seed.

sim_run_index <- 1 #Run. Set to 0 for a test.

if( sim_run_index == 0){
  commandArgs <- function(...) c("B", "10", "intf", "TRUE", "123")
}

if( sim_run_index == 1){
  commandArgs <- function(...) c("B", "30", "intf", "FALSE", "1")
}

on_cluster <- FALSE
###
if( on_cluster ){
  #get some slurm variables
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  job_id <- Sys.getenv("SLURM_JOB_ID")
  
}else{
  procs <- 20
  submit_dir <- getwd()
  job_id <- 123456789
}  
###

setwd("../files")
source("findopt_intf_doparallel.R")
