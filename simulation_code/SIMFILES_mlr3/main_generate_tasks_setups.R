#CREATES tasks for simulations for each model-dimension combination.
#SAVES tasks into folder -> to be used by main_simfile.R
#SAVES setup (indexed list of algorithm-tasks-combintion) as setup_dt.RDS. This list contains all simulations which shall be made.
#ATTENTION: make sure to run this file only once. -> use same simulated data for each algorithm
# --> Generated data used in the simulations in the paper is available in folders named "tasks_serious" (combined test/train data for use with mlr3) and "tasks_serious_list" (stored as R list objects)

#for testing: set generate_tasklist_serious to FALSE and ..._test to TRUE.

library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3tuning)
# set.seed(1256)
setwd("C:/Users/ricar/Documents/Universitaet-Projekte/Projekt-Paper-Chi-et-al/R-Code/rsrfdev/simulate_large")
source("generate_data.R")
source("util.R")
source("generate_tasks_mlr3.R")
source("util_mlr3.R")

generate_tasklist_serious <- TRUE
generate_tasklist_test <- FALSE

create_files_and_tasks <- function( algorithms, models, dimensions, nsims, n, t_dir ){
  if(!dir.exists(t_dir) ){
    print(paste("Folder", t_dir, "does not exist.") )
    dir.create(t_dir)
    print( paste("Created folder:", t_dir) )
  }
  setup_dt <- data.table::CJ(algorithm = algorithms, model = models, d = dimensions)
  setup_dt[,ID := .I]
  setup_dt[,n := n]
  setup_dt[,nsims := nsims]
  print( setup_dt )
  saveRDS(setup_dt, paste0(t_dir, "/setup_dt.RDS") )
  print("Setup files created succesfully.")
  print(paste0("Setup files are in: ",t_dir))
  ###
  task_setup_dt <- unique( setup_dt[,c("model", "d")] )
  for( i in 1:nrow(task_setup_dt)){
    ls <- generateTasklist( type = task_setup_dt[i]$model, nsims = nsims, n = n , d = task_setup_dt[i]$d )
    filename <- paste0( t_dir, "/", task_setup_dt[i]$model, toString(task_setup_dt[i]$d), ".RDS")
    saveRDS( ls , filename )
  }
  print("Task files created successfully.")
  print(paste0("Task files are in: ", t_dir))
}


#ONLY RUN ONCE before all the simulations. 
if(generate_tasklist_serious == TRUE){
  #List of simulation setups
  algorithms <- c("rsrf_nf_cv",
                  "rsrf_af_cv",
                  "ranger_cv",
                  "intf_cv",
                  "extratrees_cv",
                  "rsrf_nf_opt",
                  "rsrf_af_opt",
                  "ranger_opt",
                  "intf_opt",
                  "extratrees_opt"
  )
  models <- c("F")
  dimensions <- c(6)
  create_files_and_tasks( algorithms = algorithms,
                          models = models,
                          dimensions = dimensions, 
                          nsims = 100,
                          n = 500,
                          t_dir = "tasks_serious_new")
}  

if(generate_tasklist_test == TRUE){
  #List of simulation setups
  algorithms <- c("intf_cv")
  models <- c("A")
  dimensions <- c(4)
  create_files_and_tasks( algorithms = algorithms,
                          models = models,
                          dimensions = dimensions, 
                          nsims = 3,
                          n = 50,
                          t_dir = "tasks_test")
}  

