# source("simulate_large/util_simulations.R")
###REGRESSION FUNCTIONS###
#Simple 2-Interaction with linear component
m_interact2 <- function(x1,x2,x3,...){ return( (x1 - 0.5) * (x2 - 0.5) + x3 ) }
m_interact2_factor <- function(x1,x2,x3,factor = 10, ...){ return( factor*( (x1 - 0.5) * (x2 - 0.5) + x3 ) ) }
#OLD m_interact2_factor <- function(x1,x2,x3,factor = 10, ...){ return( factor*(x1 - 0.5) * (x2 - 0.5) + x3 ) }
#Simple sparse 2-Interaction
m_interact2_sparse <- function(x1,x2,...){ return( (x1- 0.5) * (x2 - 0.5)  )}
#Simple linear
m_lin <- function(x1,x2,x3,...){ return(x1 + x2 + x3) }
#Pure Sparse
m_pure_sparse <- function(x1,x2,x3,...){
  return( -2*sin(x1 * x2 * pi ) + 2 * sin( x2 * x3 * pi ) )
}
#Pure Dense (only dense for d=4)
m_pure_dense <- function(x1,x2,x3,x4,...){
  return( -2*sin(x1 * x2 * pi ) + 2 * sin( x2 * x3 * pi ) - 2 * sin( x3 * x4 * pi ) )
}
#Hierarchical
m_hierarchical <- function(x1,x2,x3,...){
  -2*sin(x1 * pi) + 2*sin(x2 * pi) - 2*sin(x3 * pi) -2*sin(x1 * x2 * pi ) + 2 * sin( x2 * x3 * pi )
}
#Additive
m_additive <- function(x1,x2,x3,...){
  return( -2*sin(x1 * pi) + 2*sin(x2 * pi) - 2*sin(x3 * pi) )
}

m_interact2_factor_add <- function(x1,x2,x3,x4,x5,x6,A=10, B=1,...){ return( A*( (x1 - 0.5) * (x2 - 0.5) ) + B*(x3 + x4 + x5 + x6  ) )}

###DATA GENERATING FUNCTIONS###
#Input: n, Regression function (with arbitrary number of arguments), Dimension d, type of error, additional arguments are applied to error distribution
#Output: Data Frame. Covariates are Uniform[0,1]
generateDataUnif <- function(n, m_reg, d, errortype="normal", ... ){
  m_reg_vec <- vectFun( m_reg )
  X <- runif(d * n, min = 0 , max = 1)
  dim(X) <- c(n,d)
  mx <- sapply( 1:n, function(i){ m_reg_vec( X[i,] )   } )
  colnames(X) <- paste0('x',  strrep(0, nchar(d) - nchar(1:d) ), 1:d) #Important to set colnames first AFTER applying m_reg_vec
  #Set y
  if(errortype == "normal"){
    y  <- mx + rnorm(n, ...)
  }else if(errortype == "noiseless"){
    y <- mx
  }
  
  result <- as.data.frame( X )
  result$mx <- mx
  result$y <- y 
  return( result )
}

#Input: n, Regression function (with arbitrary number of arguments), Dimension d, type of error, additional arguments are applied to error distribution
#Output Data Frame. Covariates are distributed according to Nisp data
generateDataNisp <- function(n, m_reg, d, rho = 0.3, errortype="normal",...){
  m_reg_vec <- vectFun( m_reg )
  a1 <- rnorm(d * n, 0 , sd = sqrt( 1-rho ) )
  a0 <- rnorm(n, 0, sd = sqrt( rho ) )
  X <- 2.5 * atan( a1 + a0 ) / pi
  dim(X) <- c(n,d)
  mx <- sapply( 1:n, function(i){ m_reg_vec( X[i,] )   } )
  colnames(X) <- paste0('x',  strrep(0, nchar(d) - nchar(1:d) ), 1:d) #Important to set colnames first AFTER applying m_reg_vec
  #Set y
  if(errortype == "normal"){
    y  <- mx + rnorm(n, ...)
  }else if(errortype == "noiseless"){
    y <- mx
  }
  result <- as.data.frame( X )
  result$mx <- mx
  result$y <- y 
  return( result )
  
}



###TEST/EXAMPLE
# m_proj1 <- function(x1,...){
#   return(x1)
# }
# set.seed(3)
# t1 <- generateDataUnif(5, m_reg = m_proj1, errortype="normal",d=4, sd=20)
# set.seed(3)
# t2 <- generateDataUnif(5, m_reg = m_proj1, errortype="noiseless",d=4, sd=20)
# t3 <- generateDataNisp(1, m_reg = function(...) return(0), errortype="normal",d=4, sd=1)
# t1
# t2
# t3