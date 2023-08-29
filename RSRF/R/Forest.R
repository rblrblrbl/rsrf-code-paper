##' @title Forest class
##' @description Virtual class for Random forest.
##' Contains all fields and methods used in all Forest subclasses.
##' @importFrom parallel mclapply
##' @import parallel
##' @import methods
Forest <- setRefClass("Forest",
  fields = list(
    num_trees = "integer",
    min_node_size = "integer",
    splitrule = "character",
    data = "Data",
    predict_data = "Data",
    formula = "formula",
    trees = "list",
    treetype = "character",
    randomization_step = "logical",
    use_all_data = "logical",
    replace = "logical",
    rsrf_width = "integer",
    rsrf_depth = "integer",
    mtry_mode = "character",
    mtry_rsrf_step = "integer",
    mtry_rsrf_step_random = "integer",
    mtry_cart_cart = "integer",
    fixed_cart_cart = "logical",
    include_CART_CART = "logical",
    test_mode = "logical",
    saveNodeInformation = "logical"),
  methods = list(

    grow = function(num_threads) {

      ## Init trees
      temp <- lapply(trees, function(x) {
        x$min_node_size <- min_node_size
        x$splitrule <- splitrule
        x$randomization_step <- randomization_step
        x$use_all_data <- use_all_data
        x$data <- data
        x$rsrf_width <- rsrf_width
        x$rsrf_depth <- rsrf_depth
        x$mtry_rsrf_step <- mtry_rsrf_step
        x$mtry_rsrf_step_random <- mtry_rsrf_step_random
        x$mtry_cart_cart <- mtry_cart_cart
        x$include_CART_CART <- include_CART_CART
        x$fixed_cart_cart <- fixed_cart_cart
        x$test_mode <- test_mode
        x$mtry_mode <- mtry_mode
        x$saveNodeInformation <- saveNodeInformation
      })

      ## Grow trees
      
      if(num_threads > 1){
        #Use mclapply
        trees <<- mclapply(trees, function(x) {
          x$grow(replace, use_all_data = use_all_data, rsrf_width = rsrf_width)
          x
        }, mc.cores = num_threads)
      }else{
        #Use lapply
        trees <<- lapply(trees, function(x) {
          x$grow(replace, use_all_data = use_all_data, rsrf_width = rsrf_width)
          x
        })
      }
    },

    predict = function(newdata) {
      model.data <- model.frame(formula, newdata)

      ## Save prediction data in model
      predict_data <<- Data$new(data = model.data)
      ## Predict in trees
      # predictions <- simplify2array(lapply(trees, function(x) {
      #   x$predict(predict_data)
      # }), except = NULL)
      predictions <- simplify2array(lapply(trees, function(x) {
        x$predict(predict_data)
      }))

      ## Aggregate predictions
      return(aggregatePredictions(predictions))
    },
    
   predict_wrapper_for_mlr3 = function(newdata_notarget, targetname){
      #Wrapper for the predict function within Forest.R for usage within package mlr3exraleaners. Remark: This is a ugly workaround. At some point this should become irrelevant by writing a new implementation of predict function
      #Note: Ugly coding! Solve: Write a nice predict function at some time.
      #Sets dummy cullumn named with target variable name (has to be the same as the one used in formula)
      #All values in cullumn are set to 0. Note that target variable has no effect when predicting using predict.
      newdata <- newdata_notarget
      newdata[, targetname] <- 0
      predict( newdata )
    },
    
    aggregatePredictions = function(predictions) {
      ## Empty virtual function
    },

    predictionError = function() {
      ## Empty virtual function
    },

    variableImportance = function(type = "permutation", num_threads = 1) {
      ## Calculate tree VIM
      vim_trees <- mclapply(trees, function(x) {
        x$variableImportance(type)
      }, mc.cores = num_threads)

      ## Aggregate over trees
      rowMeans(simplify2array(vim_trees))
    },
   
   get_id_count = function(){
       count_tree_ls <- lapply(trees, function(x) { x$count() } )
       count_tree <- Reduce(c, count_tree_ls )
       #use variable names from data
       count_tree <- data$names[count_tree]
       count_data_frame <- as.data.frame( table( count_tree) )
       names( count_data_frame) <- c("variable", "count")
       count_data_frame$freq <- round( count_data_frame$count /sum(count_data_frame$count) ,digits = 3)
       return( count_data_frame ) 
   },

    show = function() {
      cat("simpleRSRF Forest\n")
      cat("Type:                            ", treetype, "\n")
      cat("Splitrule:                       ", splitrule, "\n")
      cat("Number of trees:                 ", num_trees, "\n")
      cat("Sample size:                     ", data$nrow, "\n")
      cat("Number of independent variables: ", data$ncol-1, "\n")
      cat("Mtry (RSRF step):                ", mtry_rsrf_step, "\n")
      cat("Mtry (CART-CART):                ", mtry_cart_cart, "\n")
      cat("include_CART_CART:               ", include_CART_CART, "\n")
      cat("rsrf_width:                      ", rsrf_width, "\n")
      cat("rsrf_depth:                      ", rsrf_depth, "\n")
      cat("Target node size:                ", min_node_size, "\n")
      cat("Replace                          ", replace, "\n")
      cat("All data used per tree?          ", use_all_data, "\n")
      cat("OOB prediction error:            ", predictionError(), "\n")
      if( test_mode == TRUE){
        warning("Test_mode is turned on. See documentation what it does.")
      }
      
      if( saveNodeInformation == TRUE){
        df_counts <- get_id_count()
        cat("Frequency of performed splits per variable: \n")
        for( row in 1:nrow(df_counts)){
          cat("     Variable ", as.character( df_counts[row,]$variable ), ": ", df_counts[row,]$freq * 100, " % \n", sep="" )
        }
      }
      
      
      if( saveNodeInformation == TRUE){
        cat("\n\n")
        cat("Additional node information available. For a tree in the forest, check tree$nodeInformation")
      }
    },

    print = function() {
      show()
    },

    getTree = function(treenumber){
      if( !(treenumber %in% seq(1:num_trees))){
        cat("Tree with this number is not available.")
      }
      else{
        return( trees[treenumber][[1]] )
      }
    },

    print_tree = function(treenumber = 1){
      if( !(treenumber %in% seq(1:num_trees))){
        cat("Tree with this number is not available.")      }
      else{
        getTree(treenumber)$print_tree()
        cat( paste("Printed tree no. ", treenumber))
      }
    }
    )
)


