library(data.table)
library(gridExtra) #needed for printing as pdf table

file_location <- getwd() #make sure this is the location where the current file is.
setup_dt <- readRDS("setup_dt.RDS")
#Get only IDS for opt versions
setup_dt <- setup_dt[algorithm %in% c("extratrees_opt", "ranger_opt", "rsrf_af_opt", "rsrf_nf_opt", "intf_opt")]

#Reproduce all results using optimal parameters.
#set below which models shall be simulated
#A: pure-type
#B: hierarchical
#C: additive
#E: pure-2
#F: pure-3

models_chosen <- c("A")
dimensions_chosen <- c(4)
algorithms_chosen <- c("extratrees_opt", "ranger_opt", "rsrf_nf_opt", "rsrf_af_opt", "intf_opt" )
#algorithms_chosen <- c("extratrees_opt")



simulate_ids <- setup_dt[model %in% models_chosen & d %in% dimensions_chosen & algorithm %in% algorithms_chosen]$ID
opt_result_dt <- copy(setup_dt)
opt_result_dt <- opt_result_dt[ID %in% simulate_ids]
opt_result_dt[,error:=numeric(0)]
opt_result_dt[,sd:=numeric(0)]

################################
load_only <- FALSE
### Simulate given ID.

### RUN SIM
setwd( paste0( file_location, "/SIMFILES_mlr3" ) )

for( id in simulate_ids ){
  if( load_only == TRUE){
    print(paste0("Load only. ID: ", id) )
    #Read R object
    a <- readRDS( paste0( "results/result_dt_", id, ".RDS" ) )
    opt_result_dt[ID == id, error:=a$error]
    opt_result_dt[ID == id, sd:=a$sd]
  }else{
    id_simrun <- id
    isTest <- FALSE
    source("run_simulation_no_commandargs.R")
    #Read resulting R object
    a <- readRDS( paste0( "results/result_dt_", id, ".RDS" ) )
    opt_result_dt[ID == id, error:=a$error]
    opt_result_dt[ID == id, sd:=a$sd]
  }
}  
################################


#Finalize result table

all_opt_names <- c("rsrf_nf_opt", "rsrf_af_opt", "intf_opt", "ranger_opt", "extratrees_opt")
prefered_order_algo <-  all_opt_names[ all_opt_names %in% unique( opt_result_dt$algorithm ) ]

opt_result_dt[ ,error_rounded := round(error,digits=3)]
opt_result_dt[ ,sd_rounded := round(sd,digits=3)]
opt_result_dt[, algorithm_factor := factor(algorithm, levels=prefered_order_algo)]

#rename algo entries
rename_man <- function(x){
  if( x == "A") return( "pure-type (A)" )
  if( x == "B") return( "hierarchical (B)")
  if( x == "C") return( "additive (C)")
  if( x == "E") return( "pure-2 (E)" )
  if( x == "F") return( "pure-3 (F)" )
}
opt_result_dt[ , model_name := Vectorize(rename_man)(model) ]

#Order Table, first by model, then by d, then by order in all_opt_names below.
setorder(opt_result_dt, model, d, algorithm_factor )

#save table to pdf. one per model chosen.
for( model_id in models_chosen ){
  for( d_value in dimensions_chosen ){
    pdf(paste0(file_location,"/results/", rename_man(model_id), "_", d_value, ".pdf"),paper="a4", width = 10, height = 10 )   # Export PDF
    grid.table(  opt_result_dt[model == model_id & d==d_value,c("model_name", "d", "algorithm", "error_rounded", "sd_rounded")] )
    # table_g <- tableGrob( opt_result_dt[,c("model_name", "d", "error_rounded")] , rows=NULL, theme=ttheme_default(base_size=10))
    dev.off()
  }
}
