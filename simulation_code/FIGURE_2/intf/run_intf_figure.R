sim_index <- 1 #Run for 1,2, ... 9.

### Setup for 500
if( sim_index == 1){
  commandArgs <- function(...) c("100", "2323", "FALSE", "unif_pure_add", "1", "500")
}

### Setup for 1000
if( sim_index == 1){
  commandArgs <- function(...) c("100", "323", "FALSE", "unif_pure_add", "1", "1000")
}

### Setup for 2500
if( sim_index == 3){
  commandArgs <- function(...) c("100", "1123", "FALSE", "unif_pure_add", "1", "2500")
}

### Setup for 5000
if( sim_index == 4){
  commandArgs <- function(...) c("100", "953", "FALSE", "unif_pure_add", "1", "5000")
}

### Setup for 10000
if( sim_index == 5){
  commandArgs <- function(...) c("100", "5", "FALSE", "unif_pure_add", "1", "10000")
}

### Setup for 15000
if( sim_index == 6){
  commandArgs <- function(...) c("100", "132", "FALSE", "unif_pure_add", "1", "15000")
}

### Setup for 20000
if( sim_index == 7){
  commandArgs <- function(...) c("100", "142", "FALSE", "unif_pure_add", "1", "20000")
}

### Setup for 25000
if( sim_index == 8){
  commandArgs <- function(...) c("100", "542", "FALSE", "unif_pure_add", "1", "25000")
}


### Setup for 50000
if( sim_index == 9){
  commandArgs <- function(...) c("100", "52", "FALSE", "unif_pure_add", "1", "50000")
}

on_cluster <- FALSE
###
if( on_cluster ){
  #get some slurm variables
  procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
  job_id <- Sys.getenv("SLURM_JOB_ID")
  
}else{
  procs <- 25
  submit_dir <- getwd()
  job_id <- paste0(sim_index, "")
}  
###

source("simulation_intf_doparallel_figure.R")
