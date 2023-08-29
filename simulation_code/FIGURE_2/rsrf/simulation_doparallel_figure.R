#Load simpleRSRF
library(simpleRSRF)
library(doParallel)
library(doRNG)

# #if getting from console input use below, otherwise set manually in code
cli <- commandArgs(trailingOnly = TRUE)
args <- strsplit(cli, "=", fixed = TRUE)
mtry_from_input <- as.numeric( args[[1]] ) #e.g. 118
width_from_input <- as.numeric( args[[2]])
mtry_mode_from_input <- as.character( args[[3]] )
include_cart_cart_from_input <- as.logical( args[[4]])
seed_from_input <- as.numeric( args[[5]] ) #e.g. 1
is_test_from_input <- as.logical( args[[6]] ) #e.g. "TRUE"
type_from_input <- as.character( args[[7]] ) #e.g. "unif_pure_add"
sd_from_input <- as.numeric( args[[8]] ) #e.g. 1
n_from_input <- as.numeric( args[[9]] )

print(mtry_from_input)
print(width_from_input)
print(mtry_mode_from_input)
print(include_cart_cart_from_input)
print(seed_from_input)
print(is_test_from_input)
print(type_from_input)
print(sd_from_input)
#Change to FALSE for running the actual simulation, otherwise fast test-setup
test <- is_test_from_input

registerDoParallel(cores=procs)
mcoptions <- list(preschedule=FALSE)
#set seed using DoRNG
if( !is.na(seed_from_input) ){
  registerDoRNG( seed = seed_from_input, once = TRUE)
}

set.seed(seed_from_input)

#Regression functions
m_interact2_factor <- function(x1,x2,x3,x4){ return( 5*( (x1 - 0.5) * (x2 - 0.5) + x3 ) ) }
m_interact2_factor_2text <- function(){ return("5* ( (x1-0.5)(x2-0.5)+x3 )") }

#Simple 2-Interaction with several linear components with factor
m_interact2_factor_add <- function(x1,x2,x3,x4,x5,x6,A=10, B=2,...){ return( A*( (x1 - 0.5) * (x2 - 0.5) ) + B*(x3 + x4 + x5 + x6  ) )}
m_interact2_factor_add_2text <- function(A, B){ return(paste0( "A*(x1-0.5)(x2-0.5)+B*(x3 + x4 + x5 + x6)", ", A: ", A, ", B: ", B )) }

#Data generation
generateDataUnif<- function(n, m_reg, sd = 1){
  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)
  x4 <- runif(n)
  mx <- m_reg(x1, x2, x3, x4)
  y  <- mx + rnorm(n, sd = sd)
  regdata <- data.frame(x1 = x1, x2=x2, x3 = x3, x4 = x4, y = y, mx = mx )
  return( regdata )
}


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
start_simulation_rsrf <- function( parameters, m_reg, n_sims , sim_details = list( reg_function = "NA", data_type = "NA", epsilon_sd = "NA") ){
  
  
  simulate_rsrf_results <- foreach( i = icount(n_sims)) %dopar% {
    if(sim_details$data_type == "unif"){
      epsilon_sd <- sim_details$epsilon_sd
      regdata_train <- generateDataUnif(parameters$n, m_reg, sd = epsilon_sd)
    }
    if(sim_details$data_type == "unif_pure_add"){
      epsilon_sd <- sim_details$epsilon_sd
      regdata_train <- generateDataUnif06(parameters$n, m_reg, sd = epsilon_sd)
    }
    regdata_train_noise <- subset(regdata_train, select = -mx )
    if( i == 1  ){ rsrf_start_time <- Sys.time() }
    #Using simpleRSRF
    if( parameters$mtry_rsrf_step_random == "null_string"){
      mtry_rsrf_step_random_local <- NULL
    }else{
      mtry_rsrf_step_random_local <- parameters$mtry_rsrf_step_random
    }
    
    if( parameters$mtry_cart_cart == "null_string"){
      mtry_cart_cart_local <- NULL
    }else{
      mtry_cart_cart_local <- parameters$mtry_cart_cart
    }
    
    rsrf <- simpleRSRF( y ~ .,
                        data = regdata_train_noise,
                        num_threads = 1,
                        randomization = TRUE,
                        saveNodeInformation = FALSE,
                        num_trees = parameters$num_trees,
                        min_node_size = parameters$min_node_size,
                        replace = parameters$replace,
                        rsrf_width = parameters$rsrf_width,
                        mtry_mode = parameters$mtry_mode,
                        mtry_rsrf_step_random = mtry_rsrf_step_random_local,
                        mtry_rsrf_step = parameters$mtry_rsrf_step,
                        mtry_cart_cart = mtry_cart_cart_local,
                        fixed_cart_cart = parameters$fixed_cart_cart,
                        rsrf_depth = parameters$rsrf_depth,
                        include_CART_CART = parameters$include_CART_CART
    )
              
    #Calculate Errors
    if(sim_details$data_type == "unif"){
      regdata_test <- generateDataUnif(parameters$n, m_reg, sd = epsilon_sd)
    }
    if(sim_details$data_type == "unif_pure_add"){
      regdata_test <- generateDataUnif06(parameters$n, m_reg, sd = epsilon_sd)
    }
    
    predict_rsrf <- rsrf$predict( subset(regdata_test, select = -mx ) )
    simulate_rsrf <- mean( (predict_rsrf - regdata_test$mx )^2 )
    predict_train <- rsrf$predict( regdata_train_noise )
    simulate_train_rsrf <-  mean( (predict_train - regdata_train$y )^2 )
    return( list(simulate_rsrf, simulate_train_rsrf ) )
  }
  
  simulate_rsrf_test <- sapply( simulate_rsrf_results, function(ls) return( ls[[1]] ) )
  simulate_rsrf_train <- sapply( simulate_rsrf_results, function(ls) return( ls[[2]] ) )
  
  parameters$reg_function <- sim_details$reg_function
  parameters$data_type <- sim_details$data_type
  parameters$rsrf_time <- "NA"
  parameters$sims <- n_sims
  parameters$noise_sd <- sim_details$epsilon_sd
  parameters$error_rsrf <- mean( simulate_rsrf_test )
  parameters$sd_rsrf <- sd( simulate_rsrf_test )
  parameters$error_train_rsrf <- mean( simulate_rsrf_train )
  parameters$sd_train_rsrf <- sd(simulate_rsrf_train)
  #Returns all parameters and outputs
  return(parameters)
}

#Setup Regression function
if( type_from_input == "unif" ){
  m_reg <- m_interact2_factor
  m_reg_2text <- m_interact2_factor_2text
}

#Set regression function
if(type_from_input == "unif_pure_add"){
  A_val <- 10
  B_val <- 1
  m_reg <- function(x1,x2,x3,x4,x5,x6){ return( m_interact2_factor_add(x1,x2,x3,x4,x5,x6,A=A_val, B=B_val))}
  m_reg_2text <- function(){ return( m_interact2_factor_add_2text(A = A_val, B= B_val) )}
}

#Parameters simulation
if(test == FALSE){
  n <- n_from_input
  num_trees <- 100
  replace <- TRUE
  min_node_size <- 10
  replace <- TRUE
  rsrf_depth <- 2
  mtry_rsrf <- mtry_from_input
  mtry_mode <- mtry_mode_from_input
  rsrf_width <- width_from_input
  include_CART_CART <- include_cart_cart_from_input
  if( mtry_mode == "allfixed"){
    mtry_rsrf_step_random = mtry_rsrf
    mtry_rsrf_step = mtry_rsrf
    mtry_cart_cart <- "null_string"
    fixed_cart_cart <- TRUE
  }
  if( mtry_mode == "nothingfixed"){
    mtry_rsrf_step_random <- "null_string"
    mtry_rsrf_step <- mtry_rsrf
    mtry_cart_cart <- mtry_rsrf
    fixed_cart_cart <- FALSE
  }
} else { #for testing
  # n <- c(100, 200, 300)
  n <- n_from_input
  num_trees <- 10
  replace <- TRUE
  min_node_size <- 5
  rsrf_depth <- 2
  mtry_rsrf <- mtry_from_input
  mtry_mode <- mtry_mode_from_input
  rsrf_width <- width_from_input
  include_CART_CART <- include_cart_cart_from_input
  if( mtry_mode == "allfixed"){
    mtry_rsrf_step_random <- mtry_rsrf
    mtry_rsrf_step <- mtry_rsrf
    mtry_cart_cart <- "null_string"
    fixed_cart_cart <- TRUE
  }
  if( mtry_mode == "nothingfixed"){
    mtry_rsrf_step_random <- "null_string"
    mtry_rsrf_step <- mtry_rsrf
    mtry_cart_cart <- mtry_rsrf
    fixed_cart_cart <- FALSE
  }
  
  
  
}



#All Combinations
sim_parameters <- expand.grid(n = n,
                              num_trees = num_trees,
                              replace = replace,
                              min_node_size = min_node_size,
                              mtry_rsrf_step_random = mtry_rsrf_step_random,
                              mtry_rsrf_step = mtry_rsrf_step,
                              mtry_cart_cart = mtry_cart_cart,
                              mtry_mode = mtry_mode,
                              rsrf_width = rsrf_width,
                              min_node_size = min_node_size,
                              replace = replace,
                              include_CART_CART = include_CART_CART,
                              fixed_cart_cart = fixed_cart_cart,
                              rsrf_depth = rsrf_depth,
                              stringsAsFactors = FALSE
)
print(sim_parameters)
#Further simulation details
sim_details <- NULL
sim_details$reg_function <- m_reg_2text()
sim_details$data_type <- type_from_input
sim_details$epsilon_sd <- sd_from_input

###run simulation for each setup in sim_parameters, save results
ifelse(test == FALSE, n_sims <- 100, n_sims <- 7)
output_filename <- paste0( "out_", mtry_mode_from_input, "_", mtry_from_input, "_job_id_", job_id)
output_filename_txt <- paste0( output_filename, "/info_and_progress.txt")
dir.create( output_filename )

#Write information

parameters_colnames <- colnames(sim_parameters)[-1]
parameters_colnames
text_parameters<-  sapply( parameters_colnames, function(coln) { paste0( coln, strrep(" ", 30-nchar(coln)), ": ", toString(unique( sim_parameters[coln]  ) ), "\n")})
cat( "------------------------\n", 
     "          Setup         \n",
     "------------------------\n",
     "Algorithm:              ", "RSRF\n",
     "Regression function:    ", sim_details$reg_function, "\n",
     "Data type:              ", sim_details$data_type, "\n",
     "SD in error term:       ", sim_details$epsilon_sd, "\n",
     "number of data points:  ", toString( n ), "\n",
     "------------------------\n",
     "        Parameters      \n",
     "------------------------\n",
     text_parameters,
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
                                 file_progress <- paste0(submit_dir,"/progress_", job_id, ".txt")
                                 cat( "Starting simulation no. ", toString(i) , "\n", file = file_progress, append = TRUE)
				 cat("Starting time: ",format(Sys.time() ), "\n", file = file_progress, append = TRUE)
				 result <- start_simulation_rsrf( sim_parameters[i,], m_reg = m_reg, n_sims = n_sims, sim_details= sim_details )
                                 print(paste("Finished simulation no.", toString(i), " (out of ", nrow(sim_parameters), ")" ))
                                 cat( "Finished simulation no.", toString(i), " (out of ", nrow(sim_parameters), ")", "\n", file = output_filename_txt, append=TRUE )
                                 cat("Time: ", format(Sys.time()), "\n", file = output_filename_txt, append= TRUE)
                                 cat( "Finished simulation no.", toString(i), " (out of ", nrow(sim_parameters), ")", "\n", file = file_progress, append=TRUE )
                                 cat("Time: ", format(Sys.time()), "\n", file = file_progress, append= TRUE)
                                 
                                 return(result)
                               } )
#Save results
saveRDS(simulations_results, paste0(output_filename, "/simulations_results_rsrf_", mtry_mode_from_input, "_width_", width_from_input, "_mtry_", mtry_from_input, "n_", n, ".RDS") )

