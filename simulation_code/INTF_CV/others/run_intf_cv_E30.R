cv_sim_index <- 1 #Run for 1,2, ... 20. 


if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "5", "E", "30", "943", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("6", "10", "E", "30", "944", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("11", "15", "E", "30", "945", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("16", "20", "E", "30", "946", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("21", "25", "E", "30", "947", "FALSE")
}
if( cv_sim_index == 6){
  commandArgs <- function(...) c("26", "30", "E", "30", "948", "FALSE")
}
if( cv_sim_index == 7){
  commandArgs <- function(...) c("31", "35", "E", "30", "949", "FALSE")
}
if( cv_sim_index == 8){
  commandArgs <- function(...) c("36", "40", "E", "30", "950", "FALSE")
}
if( cv_sim_index == 9){
  commandArgs <- function(...) c("41", "45", "E", "30", "951", "FALSE")
}
if( cv_sim_index == 10){
  commandArgs <- function(...) c("46", "50", "E", "30", "952", "FALSE")
}
if( cv_sim_index == 11){
  commandArgs <- function(...) c("51", "55", "E", "30", "953", "FALSE")
}
if( cv_sim_index == 12){
  commandArgs <- function(...) c("56", "60", "E", "30", "954", "FALSE")
}
if( cv_sim_index == 13){
  commandArgs <- function(...) c("61", "65", "E", "30", "955", "FALSE")
}
if( cv_sim_index == 14){
  commandArgs <- function(...) c("66", "70", "E", "30", "956", "FALSE")
}
if( cv_sim_index == 15){
  commandArgs <- function(...) c("71", "75", "E", "30", "957", "FALSE")
}
if( cv_sim_index == 16){
  commandArgs <- function(...) c("76", "80", "E", "30", "958", "FALSE")
}
if( cv_sim_index == 17){
  commandArgs <- function(...) c("81", "85", "E", "30", "959", "FALSE")
}
if( cv_sim_index == 18){
  commandArgs <- function(...) c("86", "90", "E", "30", "960", "FALSE")
}
if( cv_sim_index == 19){
  commandArgs <- function(...) c("91", "95", "E", "30", "961", "FALSE")
}
if( cv_sim_index == 20){ 
  commandArgs <- function(...) c("96", "100", "E", "30", "962", "FALSE")
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
