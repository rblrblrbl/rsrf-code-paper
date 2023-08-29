cv_sim_index <- 1 #Run for 1,2, ... , 20. 

if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "5", "B", "30", "65", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("6", "10", "B", "30", "66", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("11", "15", "B", "30", "67", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("16", "20", "B", "30", "68", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("21", "25", "B", "30", "69", "FALSE")
}
if( cv_sim_index == 6){
  commandArgs <- function(...) c("26", "30", "B", "30", "70", "FALSE")
}
if( cv_sim_index == 7){
  commandArgs <- function(...) c("31", "35", "B", "30", "71", "FALSE")
}
if( cv_sim_index == 8){
  commandArgs <- function(...) c("36", "40", "B", "30", "72", "FALSE")
}
if( cv_sim_index == 9){
  commandArgs <- function(...) c("41", "45", "B", "30", "73", "FALSE")
}
if( cv_sim_index == 10){
  commandArgs <- function(...) c("46", "50", "B", "30", "74", "FALSE")
}
if( cv_sim_index == 11){
  commandArgs <- function(...) c("51", "55", "B", "30", "75", "FALSE")
}
if( cv_sim_index == 12){
  commandArgs <- function(...) c("56", "60", "B", "30", "76", "FALSE")
}
if( cv_sim_index == 13){
  commandArgs <- function(...) c("61", "65", "B", "30", "77", "FALSE")
}
if( cv_sim_index == 14){
  commandArgs <- function(...) c("66", "70", "B", "30", "78", "FALSE")
}
if( cv_sim_index == 15){
  commandArgs <- function(...) c("71", "75", "B", "30", "79", "FALSE")
}
if( cv_sim_index == 16){
  commandArgs <- function(...) c("76", "80", "B", "30", "80", "FALSE")
}
if( cv_sim_index == 17){
  commandArgs <- function(...) c("81", "85", "B", "30", "81", "FALSE")
}
if( cv_sim_index == 18){
  commandArgs <- function(...) c("86", "90", "B", "30", "82", "FALSE")
}
if( cv_sim_index == 19){
  commandArgs <- function(...) c("91", "95", "B", "30", "83", "FALSE")
}
if( cv_sim_index == 20){ 
  commandArgs <- function(...) c("96", "100", "B", "30", "84", "FALSE")
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
