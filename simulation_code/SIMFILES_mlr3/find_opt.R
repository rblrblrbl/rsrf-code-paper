library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3tuning)
library(data.table)

#Munir Server
# .libPaths( c( "~/modi_mount/r-packages" , .libPaths() ) )
# setwd("~/modi_mount/rsrfdev/simulate_large")

### Use below in case that input shall be taken from sh script. MODIFY d, type etc. below!!!
#cli <- commandArgs(trailingOnly = TRUE)
#args <- strsplit(cli, "=", fixed = TRUE)
#type_from_input <- as.character( args[[1]] ) #e.g. "A"
#d_from_input <- as.numeric( args[[2]])
#learner_name_from_input <- as.character( args[[3]] ) #e.g. "intf"
#isTest_from_input <- as.logical( args[[4]] ) #e.g. "FALSE"
#if(length(args) == 5){
#  seed_from_input <- as.numeric( args[[5]] )
#  set.seed(seed_from_input)
#}else{
#  seed_from_input <- "seed was not specified."
#}


### Use below if you want to set input in this file.
type_from_input <- "F" #e.g. "A"
d_from_input <- 6
seed_from_input <- 53661
set.seed(seed_from_input)
learner_name_from_input <- "extratrees" #e.g. "extratrees"   #One of the following: "ranger","extratrees" ( "rsrf_nf", "rsrf_af", "intf" ).
isTest_from_input <- FALSE #e.g. "FALSE"

source("generate_data.R")
source("util.R")
source("generate_tasks_mlr3.R")
source("util_mlr3.R")

future::plan(strategy = "sequential") #In RStudio.
# future::plan(strategy = "multicore") #On server.

#TEST
isTest <- isTest_from_input
#SETUP for tuning
if(isTest){
  nsims <- 30
  n <- 500
  d <- d_from_input
  batch_size <- 1
  design_type <- "random_design_points" #random_design_points or grid_search
  rd_design_size <- 1                  #Only needed when design_type == random_design_points
  #resolution <- 10                     #Only needed when design_type == grid_search
  }else{
  #Serious
  nsims <- 30
  n <- 500
  d <- d_from_input
  batch_size <- 10
  design_type <- "random_design_points" #random_design_points or grid_search
  rd_design_size <- 200                 #Only needed when design_type == random_design_points
  #resolution <- 10                     #Only needed when design_type == grid_search
  }

#Set learner
type <- type_from_input
learner_name <- learner_name_from_input
#One of the following:"rsrf_nf", "rsrf_af", "ranger", "intf", "extratrees"

#Generate Tasklist
tasklist <- generateTasklist( type = type, nsims = nsims, n = n, d = d)

output_folder <- "results_find_opt"
name <- paste0( type, d, "_", learner_name )
output_filename <- paste0(  output_folder, "/", name, ".txt")
progress_filename <- paste0( output_folder, "/progress_", name, ".txt")

if(!dir.exists(output_folder) ){
  stop(paste("Folder", output_folder, "for output files does not exist.") )
}

cat(file=output_filename,
    "Results Optimal parameter search.\n\n",
    "Type:               ", type, "\n",
    "n (sample/test size)", n, "\n",
    "d (Dimension)       ", d, "\n",
    "nsims               ", nsims, "\n",
    "Learner             ", learner_name, "\n\n",
    "Design_type:        ", design_type, "\n\n",
    "Seed used:          ", seed_from_input, "\n\n",
    "is test?            ", isTest, "\n\n",
    "------------------------------------\n\n",
    "Design_type details.\n")

if( design_type == "random_design_points"){
  cat(file=output_filename,
    "rd_design_size:    ", rd_design_size,"\n",
    "batch_size:        ", batch_size,"\n\n",
    "------------------------------------\n\n",
    "Learner Details.\n", append = TRUE
  )
}

if( design_type == "grid_search"){
  cat(file=output_filename,
    "resolution:       ", resolution, "\n",
    "batch_size:       ", batch_size, "\n\n",
    "------------------------------------\n\n",
    "Learner Details.\n", append = TRUE
  )
}


#Set Learner
if( learner_name == "rsrf_nf"){
  if(isTest){
    #TEST
    learner = lrn("regr.rsrf", mtry_mode = "nothingfixed",
                     randomization = TRUE, #do not change
                     test_mode = FALSE, #do not change
                     saveNodeInformation = FALSE, #do not change
                     rsrf_depth = 2,
                     #tune
                     mtry_rsrf_step = to_tune(1:d),
                     mtry_cart_cart = 3,
                     num_trees = to_tune( c(17,11,20) ) ,
                     replace = FALSE,
                     min_node_size = 5,
                     rsrf_width = to_tune( c(1,2,3) ),
                     include_CART_CART = TRUE 
  )}else{
    #SERIOUS.
    learner = lrn("regr.rsrf", mtry_mode = "nothingfixed",
                  randomization = TRUE, #do not change
                  test_mode = FALSE, #do not change
                  saveNodeInformation = FALSE, #do not change
                  rsrf_depth = 2,
                  num_trees = 100,
                  #tune
                  mtry_rsrf_step = to_tune( 1:d ),
                  mtry_cart_cart = to_tune( 1:d ),
                  min_node_size = to_tune( 5:30 ),
                  replace = to_tune( c(TRUE,FALSE) ),
                  rsrf_width = to_tune( 1:ifelse(d==30,30,15) ),
                  include_CART_CART = to_tune( c(TRUE,FALSE) )
    )
  }
}  
  
if( learner_name == "rsrf_af"){
    if(isTest){
      #TEST
      learner = lrn("regr.rsrf", mtry_mode = "allfixed",
                    randomization = TRUE, #do not change
                    test_mode = FALSE, #do not change
                    saveNodeInformation = FALSE, #do not change
                    rsrf_depth = 2,
                    replace = to_tune( c(TRUE,FALSE) ),
                    #tune
                    mtry_rsrf_step = 2,
                    mtry_rsrf_step_random = 1,
                    mtry_cart_cart = 3,
                    num_trees = to_tune( c(1,20) ) ,
                    min_node_size = 5,
                    rsrf_width = 2,
                    include_CART_CART = to_tune( c(TRUE,FALSE) )
      )
    }else{
      #SERIOUS.
      learner = lrn("regr.rsrf",
                    mtry_mode = "allfixed",
                    randomization = TRUE, #do not change
                    test_mode = FALSE, #do not change
                    saveNodeInformation = FALSE, #do not change
                    fixed_cart_cart = TRUE,
                    rsrf_depth = 2,
                    num_trees = 100,
                    #tune
                    mtry_rsrf_step = to_tune( 1:d ),
                    mtry_rsrf_step_random = to_tune( 1:d ),
                    min_node_size = to_tune( 5:30 ),
                    replace = to_tune( c(TRUE,FALSE) ),
                    rsrf_width = to_tune( 1:ifelse(d==30,30,15) ),
                    include_CART_CART = to_tune( c(TRUE,FALSE) )
      )
    }
}

if( learner_name == "intf"){
    if(isTest){
      #TEST
      learner = lrn("regr.intf",
                    num.trees = 500, 
                    min.node.size = to_tune(c(10,15) ), 
                    replace = TRUE,
                    npairs = 25
      )
    }else{
      #SERIOUS
      learner = lrn("regr.intf",
                    num.trees = 500, 
                    min.node.size = to_tune(5:30), 
                    replace = to_tune( c(TRUE, FALSE )),
                    npairs = to_tune( 1 : ifelse(d==4, 15, 30) )
      )
    }
}
  
if( learner_name == "ranger"){
    if(isTest){
      #TEST
      learner = lrn("regr.ranger",
                    mtry = to_tune( 1:d ),
                    num.trees = to_tune( c(7,100) ),
                    min.node.size = to_tune( c(5,11) ),
                    replace = TRUE
      )
    }else{
      #SERIOUS
      learner = lrn("regr.ranger",
                    mtry = to_tune( 1:d ),
                    num.trees = 500,
                    min.node.size = to_tune( 5:30 ),
                    replace = to_tune( c(TRUE,FALSE))
      )

    }
}

if( learner_name == "extratrees"){
  if(isTest){
    #TEST
    learner = lrn("regr.ranger",
                  mtry = d,
                  num.trees = 500,
                  min.node.size = to_tune( c(4,5) ),
                  splitrule = "extratrees",
                  num.random.splits = 1)
  }else{
    #SERIOUS
    learner = lrn("regr.ranger",
                  mtry = to_tune( 1:d ),
                  num.trees = 500,
                  min.node.size = to_tune( 5:30 ),
                  replace = to_tune( c(TRUE,FALSE)),
                  num.random.splits = to_tune( 1: 10 ),
                  splitrule = "extratrees",
                  sample.fraction = 1 #Note: for replace == FALSE, this choice makes it the original extremely random forest 
    )
    
  }
}

sink( file= output_filename, append=TRUE)
print( learner$param_set$values )
sink()
res_opt <- list()
#MAIN
cat( file = progress_filename, "PROGRESS.\n")
cat( file = progress_filename, "Seed chosen: ", seed_from_input, "\n", append=TRUE)

if( design_type == "random_design_points"){
 resolution = NULL
 #obtain search space once
 ps = learner$param_set$clone()$search_space()
 #generate single random design to be used in all simulations 
 design  = generate_design_random(ps, rd_design_size)$data
 cat( file = output_filename, append =TRUE,
      "--------------------", "\n",
      "     Design", "\n",
      "--------------------", "\n\n"
      )
 write.table(design, sep = "***", append=TRUE, file=output_filename, row.names = FALSE)
}else if( design_type == "grid_search"){ #doing grid search
  design <- NULL
}

start_time <- Sys.time()

for( i in 1:nsims){
  round_start_time <- Sys.time()
  res_opt[[i]] <-findOpt( tasklist$tasks[[i]], learner, design = design, batch_size = batch_size, resolution = resolution)
  round_end_time <- Sys.time()
  round_diff_time <- difftime( round_end_time, round_start_time)
  cat( file = progress_filename, 
       "Finished run: ", i, "\n",
       "Time taken:   ", format( round_diff_time ), "\n",
       append=TRUE)
}

end_time <- Sys.time()
diff_time <- difftime( end_time, start_time)


saveRDS( res_opt, paste0(output_folder, "/", name, "_all_results.RDS") )

cat( "Finished. Run time: ", format( diff_time ), "\n", file=output_filename, append=TRUE)

#Obtain best parameters by evaluating choosing smallest mean of regr.mse (among ID's) 
IDs <- res_opt[[1]]$ID
mean_error <- numeric(0)
for( id in IDs){
  mean_error[id] <- mean( sapply( res_opt, function( res ){ res[ID==id, regr.mse]} ) )
}
#Get best ID
a <- data.table( ID = IDs, mean_regr.mse = mean_error )
setorder( a , mean_regr.mse )
bestID <- a$ID[1]
best_mse <- a$mean_regr.mse[1]

#Check if this worked:
col <- colnames( res_opt[[1]])[ colnames(res_opt[[1]])%in% learner$param_set$ids() ]
param_tune_opt <- res_opt[[1]][ID == bestID, ..col ]
#CHECK IF NOTHING GOT MESSED UP
list_tune_opt <- lapply( 1:length(res_opt), function( i ) { return( res_opt[[i]][ ID == bestID, ..col ] ) } )
if( all ( sapply( list_tune_opt, all.equal, target = param_tune_opt ) )){
  txt_feedback <- "Found optimal parameters."
}else{
  txt_feedback <- "Error. Found different best setup. This should not happen -> Check code."
}

cat(file=output_filename,
    "--------------------------\n\n",
    txt_feedback, "\n",
    "--------------------------\n",
    "Optimal (tuned) parameters: \n\n", append=TRUE)


for( i in 1:ncol(param_tune_opt)){
  st <- colnames(param_tune_opt)[i]
  cat( file=output_filename, 
      st, "____________", toString( param_tune_opt[,..st] ),"\n\n", append=TRUE)
}
cat(file=output_filename, "--------------------------\n\n", "ID: ", bestID, "\n", "mean mse:", best_mse, "\n", append = TRUE)

cat(file=output_filename, "\n",
    "--------------------------\n\n",
    "Different style: \n\n", append=TRUE)


write.table(param_tune_opt, sep = "********", append=TRUE, file=output_filename, row.names = FALSE)
