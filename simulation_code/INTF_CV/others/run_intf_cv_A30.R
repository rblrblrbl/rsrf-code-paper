cv_sim_index <- 1 #Run for 1,2, ..., 20. 

if( cv_sim_index == 1){
  commandArgs <- function(...) c("1", "5", "A", "30", "2016", "FALSE")
}
if( cv_sim_index == 2){
  commandArgs <- function(...) c("6", "10", "A", "30", "2017", "FALSE")
}
if( cv_sim_index == 3){
  commandArgs <- function(...) c("11", "12", "A", "30", "2018", "FALSE")
}
if( cv_sim_index == 4){
  commandArgs <- function(...) c("13", "14", "A", "30", "2019", "FALSE")
}
if( cv_sim_index == 5){
  commandArgs <- function(...) c("15", "16", "A", "30", "2020", "FALSE")
}
if( cv_sim_index == 6){
  commandArgs <- function(...) c("17", "18", "A", "30", "2021", "FALSE")
}
if( cv_sim_index == 7){
  commandArgs <- function(...) c("19", "25", "A", "30", "2022", "FALSE")
}
if( cv_sim_index == 8){
  commandArgs <- function(...) c("26", "31", "A", "30", "2023", "FALSE")
}
if( cv_sim_index == 9){
  commandArgs <- function(...) c("32", "37", "A", "30", "2024", "FALSE")
}
if( cv_sim_index == 10){
  commandArgs <- function(...) c("38", "43", "A", "30", "2025", "FALSE")
}
if( cv_sim_index == 11){
  commandArgs <- function(...) c("44", "49", "A", "30", "2026", "FALSE")
}
if( cv_sim_index == 12){
  commandArgs <- function(...) c("50", "55", "A", "30", "2027", "FALSE")
}
if( cv_sim_index == 13){
  commandArgs <- function(...) c("56", "61", "A", "30", "2028", "FALSE")
}
if( cv_sim_index == 14){
  commandArgs <- function(...) c("62", "67", "A", "30", "2029", "FALSE")
}
if( cv_sim_index == 15){
  commandArgs <- function(...) c("68", "73", "A", "30", "2030", "FALSE")
}
if( cv_sim_index == 16){
  commandArgs <- function(...) c("74", "79", "A", "30", "2031", "FALSE")
}
if( cv_sim_index == 17){
  commandArgs <- function(...) c("80", "85", "A", "30", "2032", "FALSE")
}
if( cv_sim_index == 18){
  commandArgs <- function(...) c("86", "91", "A", "30", "2033", "FALSE")
}
if( cv_sim_index == 19){
  commandArgs <- function(...) c("92", "97", "A", "30", "2034", "FALSE")
}
if( cv_sim_index == 20){
  commandArgs <- function(...) c("98", "100", "A", "30", "2035", "FALSE")
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
