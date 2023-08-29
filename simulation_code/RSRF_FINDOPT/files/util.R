#Utility for simulation
vectFun<- function(fun){
  f_vec <- function(vec){
      return( do.call(fun, args = as.list( vec ) ) ) 
  } 
  return( f_vec )
}
