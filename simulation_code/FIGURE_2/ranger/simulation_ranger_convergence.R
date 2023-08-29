#Load ranger
library(ranger)

# #if getting from console input use below, otherwise set manually in code
# cli <- commandArgs(trailingOnly = TRUE)
# args <- strsplit(cli, "=", fixed = TRUE)
# mtry_from_input <- as.numeric( args[[1]] ) #e.g. 118
# seed_from_input <- as.numeric( args[[2]] ) #e.g. 1
# is_test_from_input <- as.logical( args[[3]] ) #e.g. "TRUE"
# type_from_input <- as.character( args[[4]] ) #e.g. "unif"
# sd_from_input <- as.numeric( args[[5]] ) #e.g. 1

seed_from_input <- mtry_from_input
is_test_from_input <- FALSE
type_from_input <- "unif_pure_add"
sd_from_input <- 1

#Change to FALSE for running the actual simulation, otherwise fast test-setup
test <- FALSE

#procs <- as.numeric(Sys.getenv("SLURM_NTASKS"))
#submit_dir <- Sys.getenv("SLURM_SUBMIT_DIR")
# job_id <- Sys.getenv("SLURM_JOB_ID")
# print(job_id)

set.seed(seed_from_input)

#Simple 2-Interaction with several linear components with factor
m_interact2_factor_add <- function(x1,x2,x3,x4,x5,x6,A=10, B=2,...){ return( A*( (x1 - 0.5) * (x2 - 0.5) ) + B*(x3 + x4 + x5 + x6  ) )}
m_interact2_factor_add_2text <- function(A, B){ return(paste0( "A*(x1-0.5)(x2-0.5)+B*(x3 + x4 + x5 + x6)", ", A: ", A, ", B: ", B )) }

#Data generation
generateDataUnif06 <- function(n, m_reg, sd = 1){
  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)
  x4 <- runif(n)
  x5 <- runif(n)
  x6 <- runif(n)
  mx <- m_reg(x1, x2, x3, x4,x5,x6)
  y  <- mx + rnorm(n, sd = sd )
  regdata <- data.frame(x1 = x1, x2=x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6, y = y, mx = mx )
  return( regdata )
}


#Function which starts a simulation run given set of (flexible) parameters
start_simulation_ranger <- function( parameters, m_reg, n_sims , sim_details = list( reg_function = "NA", data_type = "NA", epsilon_sd = "NA") ){
  simulate_rf <- NULL
  simulate_train_rf <- NULL
  for( i in (1:n_sims)){
    if(sim_details$data_type == "unif_pure_add"){
      epsilon_sd <- sim_details$epsilon_sd
      regdata_train <- generateDataUnif06(parameters$n, m_reg, sd = epsilon_sd)
    }
    regdata_train_noise <- subset(regdata_train, select = -mx )
    if( i == 1  ){ rf_start_time <- Sys.time() }
    #Using ranger
    rf <- ranger(y ~ .,
                 data = regdata_train_noise,
                 num.trees = parameters$num_trees,
                 min.node.size = parameters$min_node_size,
                 mtry = parameters$mtry_rf,
                 replace = parameters$replace)
    if( i == 1 ){  rf_end_time <- Sys.time();  rf_time <- difftime(rf_end_time,rf_start_time, units="min")  ; }

    #Calculate Errors
    if(sim_details$data_type == "unif_pure_add"){
      regdata_test <- generateDataUnif06(parameters$n, m_reg, sd = epsilon_sd)
    }
    predict_rf <- predict( rf, subset(regdata_test, select = -mx ) )$predictions
    simulate_rf[i] <- mean( (predict_rf - regdata_test$mx )^2 )
    #Train error
    predict_train <- predict( rf, regdata_train_noise )$predictions
    simulate_train_rf[i] <-  mean( (predict_train - regdata_train$y )^2 )
  }
  parameters$reg_function <- sim_details$reg_function
  parameters$data_type <- sim_details$data_type
  parameters$rf_time <- rf_time
  parameters$sims <- n_sims
  parameters$error_rf <- mean( simulate_rf )
  parameters$sd_rf <- sd( simulate_rf )
  parameters$error_train_rf <- mean( simulate_train_rf )
  parameters$sd_train_rf <- sd(simulate_train_rf)
  #Returns all parameters and outputs
  return(parameters)
}

#Setup Regression function
if(type_from_input == "unif_pure_add"){
    A_val <- 10
    B_val <- 1
    m_reg <- function(x1,x2,x3,x4,x5,x6){ return( m_interact2_factor_add(x1,x2,x3,x4,x5,x6,A=A_val, B=B_val))}
    m_reg_2text <- function(){ return( m_interact2_factor_add_2text(A = A_val, B= B_val) )}
}

#Parameters simulation
if(test == FALSE){
  n <- c(500, 1000, 2500, 5000, 10000, 15000, 20000, 25000, 50000)
  num_trees <- 100
  min_node_size <- 5
  replace <- TRUE
  mtry_rf <- mtry_from_input
} else { #for testing
  n <- c(100, 200, 300)
  num_trees <- 5
  min_node_size <- 5
  replace <- c(TRUE, FALSE)
  mtry_rf <- mtry_from_input
}

#All Combinations
sim_parameters <- expand.grid(n = n,
                              num_trees = num_trees,
                              replace = replace,
                              min_node_size = min_node_size,
                              mtry_rf = mtry_rf
                )
#Further simulation details
sim_details <- NULL
sim_details$reg_function <- m_reg_2text()
sim_details$data_type <- type_from_input
sim_details$epsilon_sd <- sd_from_input

###run simulation for each setup in sim_parameters, save results
ifelse(test == FALSE, n_sims <- 100, n_sims <- 7)
# output_filename <- paste0( "out_mtry_", mtry_from_input, "_job_id", job_id)
# output_filename_txt <- paste0( output_filename, "/info_and_progress.txt")
# dir.create( output_filename )
# output_filename <- paste0( "out_mtry_", mtry_from_input, "_job_id", job_id)
output_filename_txt <- paste0( output_filename, "/info_and_progress_", mtry_from_input, ".txt")

#Write information


cat( "------------------------\n", 
     "          Setup         \n",
     "------------------------\n",
     "Algorithm:              ", "Random Forest\n",
     "Regression function:    ", sim_details$reg_function, "\n",
     "Data type:              ", sim_details$data_type, "\n",
     "Variance in error term: ", sim_details$epsilon_sd, "\n",
     "number of data points:  ", toString( n ), "\n",
     "------------------------\n",
     "        Parameters      \n",
     "------------------------\n",
     "num trees:         ", toString( num_trees ), "\n",
     "min_node_size:     ", toString( min_node_size ), "\n",
     "replace:           ", toString( replace ), "\n",
     "mtry:              ", toString( mtry_rf ), "\n",
     "------------------------\n",
     "         Seed           \n",
     "------------------------\n",
     "Seed chosen:       ", seed_from_input, "\n\n", 
     "------------------------\n",
     "         Test?          \n",
     "------------------------\n",
     "is test? ", test, "\n\n",
     "------------------------\n",
     "        STARTING        \n",
     "------------------------\n",
     file = output_filename_txt, append = FALSE)
     
     

#Run simulations
simulations_results <- sapply( 1:nrow(sim_parameters),
                               function(i){
                                 result <- start_simulation_ranger( sim_parameters[i,], m_reg = m_reg, n_sims = n_sims, sim_details= sim_details )
                                 print(paste("Finished simulation no.", toString(i), " (out of ", nrow(sim_parameters), ")" ))
                                  cat( "Finished simulation no.", toString(i), " (out of ", nrow(sim_parameters), ")", "\n", file = output_filename_txt, append=TRUE )
                                  cat("Time: ", format(Sys.time()), "\n", file = output_filename_txt, append= TRUE)
                                  # cat( "Finished simulation no.", toString(i), " (out of ", nrow(sim_parameters), ")", "\n", file = paste0(Sys.getenv("SLURM_SUBMIT_DIR"), "/progress_", job_id, ".txt"), append=TRUE )
                                  # cat("Time: ", format(Sys.time()), "\n", file = paste0(Sys.getenv("SLURM_SUBMIT_DIR"), "/progress_", job_id, ".txt"), append= TRUE)
                                 
                                 return(result)
                               } )
#Save results
saveRDS(simulations_results, paste(output_filename, "/simulations_results_ranger_", "mtry_", mtry_from_input, ".RDS", sep="") )
