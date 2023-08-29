#Needs package mlr3
#A: pure interaction
#B: hierarchical
#C: additive
#D: Modified example from paper "High Dimensional Properties of Random Forests" (old)
#E: Modified example from paper "High Dimensional Properties of Random Forests"
#F: pure-add: Modified example from paper "High Dimensional Properties of Random Forests" with several additive components


generateTasklist <- function( type, nsims, n, d){
  #pure interaction
  ret <- list()
  for( i in 1:nsims){
    if( type == "A"){
      m_reg <- m_pure_sparse
      train<- subset( generateDataNisp( n = n, d = d, m_reg = m_reg), select = -mx)
      eval <- subset( generateDataNisp( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
      t <- as_task_regr( train, target = "y" )
      t$set_row_roles( 1:n, roles = c("use"))
      t$rbind( eval )
      t$set_row_roles( (n+1):(2*n), roles= c("holdout") )
      ret[[i]] <- t
    }
    if( type == "B"){
      m_reg <- m_hierarchical
      train<- subset( generateDataNisp( n = n, d = d, m_reg = m_reg), select = -mx)
      eval <- subset( generateDataNisp( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
      t <- as_task_regr( train, target = "y" )
      t$set_row_roles( 1:n, roles = c("use"))
      t$rbind( eval )
      t$set_row_roles( (n+1):(2*n), roles= c("holdout") )
      ret[[i]] <- t
    }
    if( type == "C"){
      m_reg <- m_additive
      train<- subset( generateDataNisp( n = n, d = d, m_reg = m_reg), select = -mx)
      eval <- subset( generateDataNisp( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
      t <- as_task_regr( train, target = "y" )
      t$set_row_roles( 1:n, roles = c("use"))
      t$rbind( eval )
      t$set_row_roles( (n+1):(2*n), roles= c("holdout") )
      ret[[i]] <- t
    }
    if( type == "D"){
      m_reg <- function(x1,x2,x3, ...) return( m_interact2_factor( factor = 3, x1,x2,x3))
      train<- subset( generateDataUnif( n = n, d = d, m_reg = m_reg), select = -mx)
      eval <- subset( generateDataUnif( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
      t <- as_task_regr( train, target = "y" )
      t$set_row_roles( 1:n, roles = c("use"))
      t$rbind( eval )
      t$set_row_roles( (n+1):(2*n), roles= c("holdout") )
      ret[[i]] <- t
    }
    if( type == "E"){
      m_reg <- function(x1,x2,x3, ...) return( m_interact2_factor( factor = 5, x1,x2,x3))
      train<- subset( generateDataUnif( n = n, d = d, m_reg = m_reg), select = -mx)
      eval <- subset( generateDataUnif( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
      t <- as_task_regr( train, target = "y" )
      t$set_row_roles( 1:n, roles = c("use"))
      t$rbind( eval )
      t$set_row_roles( (n+1):(2*n), roles= c("holdout") )
      ret[[i]] <- t
    }
    if( type == "F"){
      m_reg <- function(x1,x2,x3,x4,x5,x6,...) return( m_interact2_factor_add( A = 10, B=1, x1,x2,x3,x4,x5,x6))
      train<- subset( generateDataUnif( n = n, d = d, m_reg = m_reg), select = -mx)
      eval <- subset( generateDataUnif( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
      t <- as_task_regr( train, target = "y" )
      t$set_row_roles( 1:n, roles = c("use"))
      t$rbind( eval )
      t$set_row_roles( (n+1):(2*n), roles= c("holdout") )
      ret[[i]] <- t
    }
  }
  #Return List with tasks and details
  return( list(tasks = ret, nsims = nsims, n = n, model_index = type, d = d) )
}


generateTasklistManualUnif <- function( m, model_index_name = "manually chosen", nsims, n, d ){
    ret <- list()
    for( i in 1:nsims){
        m_reg <- m
        train<- subset( generateDataNisp( n = n, d = d, m_reg = m_reg), select = -mx)
        eval <- subset( generateDataNisp( n = n, d = d, m_reg = m_reg, errortype = "noiseless"), select = -mx )
        t <- as_task_regr( train, target = "y" )
        t$set_row_roles( 1:n, roles = c("use"))
        t$rbind( eval )
        t$set_row_roles( (n+1):(2*n), roles= c("holdout") )
        ret[[i]] <- t
    }
  return( list(tasks = ret, nsims = nsims, n = n, model_index = model_index_name, d = d) )
}
