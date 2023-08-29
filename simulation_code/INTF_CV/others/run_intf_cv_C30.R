cv_sim_index <- 1 #Run for 1,2, ... 20. 


if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "5", "C", "30", "11065", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("6", "10", "C", "30", "11066", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("11", "15", "C", "30", "11067", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("16", "20", "C", "30", "11068", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("21", "25", "C", "30", "11069", "FALSE")
}
if( cv_sim_index == 6){
  commandArgs <- function(...) c("26", "30", "C", "30", "11070", "FALSE")
}
if( cv_sim_index == 7){
  commandArgs <- function(...) c("31", "35", "C", "30", "11071", "FALSE")
}
if( cv_sim_index == 8){
  commandArgs <- function(...) c("36", "40", "C", "30", "11072", "FALSE")
}
if( cv_sim_index == 9){
  commandArgs <- function(...) c("41", "45", "C", "30", "11073", "FALSE")
}
if( cv_sim_index == 10){
  commandArgs <- function(...) c("46", "50", "C", "30", "11074", "FALSE")
}
if( cv_sim_index == 11){
  commandArgs <- function(...) c("51", "55", "C", "30", "11075", "FALSE")
}
if( cv_sim_index == 12){
  commandArgs <- function(...) c("56", "60", "C", "30", "11076", "FALSE")
}
if( cv_sim_index == 13){
  commandArgs <- function(...) c("61", "65", "C", "30", "11077", "FALSE")
}
if( cv_sim_index == 14){
  commandArgs <- function(...) c("66", "70", "C", "30", "11078", "FALSE")
}
if( cv_sim_index == 15){
  commandArgs <- function(...) c("71", "75", "C", "30", "11079", "FALSE")
}
if( cv_sim_index == 16){
  commandArgs <- function(...) c("76", "80", "C", "30", "11080", "FALSE")
}
if( cv_sim_index == 17){
  commandArgs <- function(...) c("81", "85", "C", "30", "11081", "FALSE")
}
if( cv_sim_index == 18){
  commandArgs <- function(...) c("86", "90", "C", "30", "11082", "FALSE")
}
if( cv_sim_index == 19){
  commandArgs <- function(...) c("91", "95", "C", "30", "11083", "FALSE")
}
if( cv_sim_index == 20){ 
  commandArgs <- function(...) c("96", "100", "C", "30", "11084", "FALSE")
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
