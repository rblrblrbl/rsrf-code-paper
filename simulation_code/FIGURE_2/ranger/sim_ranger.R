onCluster <- TRUE

if(onCluster){
  job_id <- Sys.getenv("SLURM_JOB_ID")
  output_filename <- "ranger_plot"
} else {
  
  job_id <- 123456789
  output_filename <- "ranger_plot"
}

dir.create(output_filename)
#Generate result files. Will be saved in folder with name "output_filename"
mtry_from_input <- 1
source("simulation_ranger_convergence.R")
mtry_from_input <- 2
source("simulation_ranger_convergence.R")
mtry_from_input <- 3
source("simulation_ranger_convergence.R")
mtry_from_input <- 4
source("simulation_ranger_convergence.R")
mtry_from_input <- 5
source("simulation_ranger_convergence.R")
mtry_from_input <- 6
source("simulation_ranger_convergence.R")

source("makeplot_rf.R")
