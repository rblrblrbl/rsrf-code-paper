##' Basic Implementation of RSRF Forests.
##' Uses reference classes and only plain \code{R}.
##' Not optimized for computation speed.
##'
##'
##' @title simpleRSRF
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit.
##' @param data Training data of class \code{data.frame}.
##' @param num_trees Number of trees.
##' @param replace Sample with replacement. Default TRUE.
##' @param randomization If TRUE, using Random-CART steps (also see rsrf_width parameter). If FALSE, algorithm is Random Forest. Default TRUE.
##' @param min_node_size Minimal node size. Default 5 for regression
##' @param mtry_mode Determines which split variables are used within the Random-CART steps. "nothingfixed" means that whenever a split is placed a new variable to split is drawn. "allfixed" means that possible split variable IDs remain unchanged over all candidate Random-CART split candidates (rsrf_width many), in every step. "semifixed" means that the possible split variables for a Random split remain fixed over candidate Random-CART-splits while the CART split variable ID are not fixed. Currently, "allfixed" and "semifixed" is only implemented for rsrf_depth = 2.
##' @param mtry_rsrf_step_random Only needed when mtry_mode = "fixed". Number of variables to possibly split at in each node in the Random step of a Random-CART-Step
##' @param mtry_rsrf_step Number of variables to possibly split at in each node in the CART step of a Random-CART-Step
##' @param mtry_cart_cart Number of variables to possibly split at in each node in a CART-CART-Step
##' @param fixed_cart_cart TRUE or FALSE. If TRUE: In mtry_mode "allfixed" (or "semifixed") the CART-CART-step variable IDs considered are the same as for Random Steps. (Only when include_CART_CART is TRUE, otherwise this parameter will be ignored.) Note that mtry_CART_CART will not be used.
##' @param rsrf_width Number of candidate Random-CART steps. (CART-CART step is not counted).
##' @param rsrf_depth Depth of Random-CART-steps. First, rsrf_depth - 1 Random splits are placed, followed by a CART split.
##' @param include_CART_CART If TRUE, we include a CART-CART step to the candidate splits of a current terminal node. Default TRUE.
##' @param splitrule Splitrule to use in trees. Defaults to "Variance" for regression forests. No other implemented.
##' @param num_threads Number of threads used for mclapply, set to 1 for debugging.
##' @param use_all_data Only use this when needed for testing. Default FALSE. If TRUE, all data points used to built a tree (no bootstrap/subsampling)
##' @param probability Not implemented. Grow a probability forest. Default FALSE.
##' @param test_mode Only for testing purposes. Sets rsrf_width to 3 if not supplied, randomization to TRUE, include_CART_CART to TRUE. In test_mode, we will not split whenvever we would have placed a split in rsrf-step. Result should be same as Random Forest. mtry_cart_cart takes role of mtry.
##' @param saveNodeInformation Can be used for testing purposes. Will save additional information for each node. Generates a field for the class tree called node_information.
##' @examples
##' \donttest{
##' library(simpleRSRF)
##'
##' # Regression
##' n <- 100
##' x1 <- runif(n, min = -1.25,max = 1.25)
##' x2 <- runif(n, min = -1.25,max = 1.25)
##' x3 <- runif(n, min = -1.25,max = 1.25)
##' y <- -2*sin(x1*x2*pi) + 2*sin(x2*x3*pi) + rnorm(n, sd=0.5)
##' train_data <- data.frame(x1 = x1, x2=x2, x3=x3, y = y)
##' rsrf <- simpleRSRF( y ~ ., train_data , mtry_cart_cart = 2, mtry_rsrf_step = 2, rsrf_width = 5)
##' # Prediction
##' n_pred <- 10
##' x1_pred <- runif(n, min = -1.25,max = 1.25)
##' x2_pred <- runif(n, min = -1.25,max = 1.25)
##' x3_pred <- runif(n, min = -1.25,max = 1.25)
##' y_pred <- -2*sin(x1_pred*x2_pred*pi) + 2*sin(x2_pred*x3_pred*pi) + rnorm(n_pred, sd=0.5)
##' pred_data <- data.frame(x1 = x1_pred, x2=x2_pred, x3=x3_pred, y = y_pred)
##' predictions_rsrf <- rsrf$predict(pred_data)

##' }
##'
##' @author Ricardo Blum
##' @references
##' Breiman, L. (2001). Random forests. Mach Learn, 45(1), 5-32. \cr
##' @import stats
##' @export
##'
simpleRSRF <- function(formula, 
                       data, 
                       num_trees = 50, 
                       mtry_mode = "nothingfixed", 
                       mtry_rsrf_step_random = NULL, 
                       mtry_rsrf_step = NULL, 
                       mtry_cart_cart = NULL,
                       min_node_size = NULL,
                       replace = TRUE,
                       probability = FALSE,
                       splitrule = NULL,
                       num_threads = 1,
                       randomization = TRUE,
                       use_all_data = FALSE,
                       rsrf_width = NULL,
                       rsrf_depth = 2,
                       include_CART_CART = TRUE,
                       fixed_cart_cart = FALSE,
                       test_mode = FALSE,
                       saveNodeInformation = FALSE) {

  model.data <- model.frame(formula, data)

  if (class(model.data[, 1]) == "factor") {
    if (probability) {
      treetype <- "Probability"
    } else {
      treetype <- "Classification"
    }
  } else if (class(model.data[, 1]) == "numeric") {
    treetype <- "Regression"
  } else if (class(model.data[, 1]) == "Surv") {
    treetype <- "Survival"
  } else {
    stop("Unkown response type.")
  }

  if( !is.null(mtry_cart_cart) && mtry_cart_cart > ncol(model.data) - 1){
    stop("mtry_cart_cart cannot be larger than number of variables.")
  }
  
  if( !is.null(mtry_rsrf_step) && mtry_rsrf_step > ncol(model.data) - 1){
    stop("mtry_rsrf_step cannot be larger than number of variables.")
  }
  
  if( !is.null(mtry_rsrf_step_random) && mtry_rsrf_step > ncol(model.data) - 1){
    stop("mtry_rsrf_step_random cannot be larger than number of variables.")
  }
  
  if (is.null(min_node_size)) {
    if (treetype == "Classification") {
      min_node_size <- 1
    } else if (treetype == "Probability") {
      min_node_size <- 10
    } else if (treetype == "Regression") {
      min_node_size <- 5
    } else if (treetype == "Survival") {
      min_node_size <- 3
    }
  }
  
  #test_mode == TRUE
  if(test_mode == TRUE){
    include_CART_CART <- TRUE
    if(is.null(rsrf_width)){
      rsrf_width <- 3
      print("rsrf_width set to 3. (test_mode)")
    }
    randomization <- TRUE
    cat("Test mode activated!\n", "Setting rsrf_width to 3 if not supplied, randomization to TRUE, include_CART_CART to TRUE.\n",
      "In test_mode, we will not split whenvever we would have placed a split in rsrf-step.\n",
      "Result should be same as Random Forest.\n", "mtry_cart_cart takes role of mtry.")
  }else{
      test_mode <- FALSE
  }
    
  if(randomization == FALSE){
    fixed_cart_cart <- FALSE
    #RF Mode. rsrf_width etc. not relevant.
    print("Randomization is turned off. Building Random Forest. mtry_cart_cart takes role of mtry.")
    if(is.null(mtry_cart_cart)){
      mtry_cart_cart <- ncol(model.data)-1
      print("Setting mtry_cart_cart to feature dimension because value for mtry_cart_cart was not supplied")
    }
  }else{
    #RSRF Mode
    if(is.null(rsrf_width)){
     stop("If randomization == TRUE, the rsrf_width parameter (>= 1) needs to be supplied. It may also be set to 0, then a Random Forest is built with mtry_cart_cart taking role of mtry.")
    }
    if(rsrf_width == 0){
      print("RSRF Forest will be exactly Random Forests (because rsrf_width is set to zero).")
      print("mtry_cart_cart takes role of mtry.")
      if(is.null(mtry_cart_cart)){
        mtry_cart_cart <- ncol(model.data)-1
        print("Setting mtry_cart_cart to feature dimension because value for mtry_cart_cart was not supplied")
      }
      if(include_CART_CART == FALSE){
        stop("include_CART_CART has to be TRUE when rsrf_width == 0")
      }
    }
    if(rsrf_width >=1){
      if(is.null(mtry_rsrf_step)){
        mtry_rsrf_step <- ncol(model.data) - 1
        print("Setting mtry_rsrf_step to feature dimension because value was not supplied")
      }
      if(mtry_mode == "allfixed" || mtry_mode =="semifixed"){
        if(is.null(mtry_rsrf_step_random)){
          mtry_rsrf_step_random <- 1
          print("Setting mtry_rsrf_step_random to 1 because value was not supplied")
        }
        if(rsrf_depth > 2){
          stop("mtry_mode allfixed and semifixed only implemented for rsrf_depth = 2")
        }
      }
    }
    if(include_CART_CART == TRUE){
      if(fixed_cart_cart == TRUE){
        if(mtry_mode == "nothingfixed"){
          stop("fixed_cart_cart set to TRUE is not allowed to be true when mtry_mode is nothingfixed.") 
        }
        if(mtry_mode == "allfixed"){
          print("mtry_mode is allfixed and fixed_cart_cart is set to True -> Ignoring mtry_cart_cart.")
          mtry_cart_cart <- -1 #This is just a dummy setting. Will not be needed.
        }
        if(mtry_mode == "semifixed"){
          if(is.null(mtry_cart_cart)){
            mtry_cart_cart <- ncol(model.data)-1
            print("Setting mtry_cart_cart to feature dimension because value was not supplied")
          }
          print("Note: mtry_cart_cart in mtry_mode = semifixed is only needed for second splits.")
          }
        }
      }else{
      print("include_CART_CART is set to False. Ignoring mtry_cart_cart and fixed_cart_cart parameters.")
    }
    
  }
    
  ## Splitrule
  if (is.null(splitrule)) {
    if (treetype == "Classification") {
      splitrule <- "Gini"
    } else if (treetype == "Probability") {
      splitrule <- "Gini"
    } else if (treetype == "Regression") {
      splitrule <- "Variance"
    } else if (treetype == "Survival") {
      splitrule <- "Logrank"
    }
  }

  ## Create forest object
  if (treetype == "Classification") {
    stop(paste("Treetype", treetype, "is not implemented in this version."))
  } else if (treetype == "Probability") {
    stop(paste("Treetype", treetype, "is not implemented in this version."))
  } else if (treetype == "Regression") {
    if(randomization == TRUE){
    print("Building RSRF Forest.")
    }else{
      print("Randomization is set to FALSE. Building Random Forest.")
    }
    forest <- ForestRandomRegression$new(num_trees = as.integer(num_trees),
                                         mtry_mode = mtry_mode,
                                         mtry_rsrf_step = as.integer(mtry_rsrf_step),
                                         mtry_rsrf_step_random = as.integer(mtry_rsrf_step_random),
                                         mtry_cart_cart = as.integer(mtry_cart_cart),
                                         min_node_size = as.integer(min_node_size),
                                         replace = replace, 
                                         splitrule = splitrule,
                                         data = Data$new(data = model.data),
                                         formula = formula,
                                         randomization_step = randomization,
                                         use_all_data = use_all_data,
                                         rsrf_width = as.integer(rsrf_width),
                                         rsrf_depth = as.integer(rsrf_depth),
                                         fixed_cart_cart = fixed_cart_cart,
                                         include_CART_CART = include_CART_CART,
                                         test_mode = test_mode,
                                         saveNodeInformation = saveNodeInformation
                                     )
  } else if (treetype == "Survival") {
    stop(paste("Treetype", treetype, "is not implemented in this version."))
  } else {
    stop("Unkown tree type.")
  }

  ## Grow forest
  forest$grow(num_threads = num_threads)

  ## Return forest
  return(forest)
}
