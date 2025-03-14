#This file adds the 50 noisy additional covariates to the datasets.
#Input: e.g. dataset_airfoil.RDS
#Output: dataset_airfoil_hd.RDS (the HD variants used in the paper)
#Seeds used here: see below.
name_ds <- "airfoil"
dataset_input <- readRDS(paste0("dataset_",name_ds,".RDS"))
ds_new <- dataset_input$dataset
rownames(ds_new) <- NULL
n <- dataset_input$n_data
##seeds used when generating additional covariates
# seed_gen_noise <- 1   #concrete
seed_gen_noise <- 222 #airfoil
# seed_gen_noise <- 630 #robot
# seed_gen_noise <- 914 #abalone
# seed_gen_noise <- 149 #california housing

set.seed(seed_gen_noise)

#generate n*N new normal covariables which are correlated
N <- 50
weight_mult <- function( vec, w0,w1,w2 ){
  return( sum( c(w2,w1,w0,w1,w2) * vec )  )
}
w0 <- 1/2
w1 <- sqrt( (1-w0^2)/2 ) #guarantees that new correlated variables have variance 1.a 
w2 <- 0
norm_matrix <- matrix( rnorm( (N+4) * n ), nrow = n )
simX <- matrix( 0,n,N)
for( j in 1:nrow(simX) ){
  simX[j,]  <- sapply( 1:N, function(x){
    return( weight_mult( vec = norm_matrix[j,seq(x,x+4)],w0,w1,w2))
  } )
}

### Info
cat("Covariances\n",
    "-4: ", w2^2, "\n",
    "-3: ", 2*w1*w2, "\n",
    "-2: ", 2*w0*w2 + w1^2, "\n",
    "-1: ", 2*w1*w2+ 2*w0*w1, "\n",
    "Variance:  ", 2*w2^2 + 2*w1^2 + w0^2) 
###
###new dataset.
for( j in 1:N){
  ds_new[,paste0("xnoise",sprintf("%02d", j) )] <- simX[,j]
}
dataset_new <- dataset_input
dataset_new$dataset <- ds_new
dataset_new$num_covariable <- dataset_new$num_covariable + N
dataset_new$N <- N
dataset_new$seed_gen_noise <- seed_gen_noise
dataset_new$data_id <- paste0(dataset_new$data_id, "_hd")
saveRDS( dataset_new, paste0("dataset_",name_ds,"_hd.RDS") )