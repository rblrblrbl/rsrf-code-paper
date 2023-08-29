sim_index <- 0 #Run for 1,2, ... 9. Set to 0 for a test.

### TODO!!!
### Setup for 500
if( sim_index == 1){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "854", "FALSE", "unif_pure_add", "1", "500")
}

### TODO!!!
### Setup for 1000
if( sim_index == 2){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "123", "FALSE", "unif_pure_add", "1", "1000")
}


### TODO!!!
### Setup for 2500
if( sim_index == 3){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "91", "FALSE", "unif_pure_add", "1", "2500")
}

### Setup for 5000
if( sim_index == 4){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "8887", "FALSE", "unif_pure_add", "1", "5000")
}

### Setup for 10000
if( sim_index == 5){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "1429", "FALSE", "unif_pure_add", "1", "10000")
}

### Setup for 15000
if( sim_index == 6){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "42", "FALSE", "unif_pure_add", "1", "15000")
}

### Setup for 20000
if( sim_index == 7){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "42", "FALSE", "unif_pure_add", "1", "20000")
}

### Setup for 25000
if( sim_index == 8){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "951", "FALSE", "unif_pure_add", "1", "25000")
}


### Setup for 50000
if( sim_index == 9){
  commandArgs <- function(...) c("4", "15", "allfixed", "FALSE", "42", "FALSE", "unif_pure_add", "1", "50000")
}

### TEST
if( sim_index == 0){
  commandArgs <- function(...) c("4", "3", "allfixed", "FALSE", "111", "TRUE", "unif_pure_add", "1", "100")
}

on_cluster <- FALSE
###
if( on_cluster ){
  #get some slurm variables
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  job_id <- Sys.getenv("SLURM_JOB_ID")
  
}else{
  procs <- 50
  submit_dir <- getwd()
  job_id <- paste0(sim_index, "")
}  
###

source("simulation_doparallel_figure.R")
