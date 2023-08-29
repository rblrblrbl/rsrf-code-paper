#Munir Server
# .libPaths( c( "~/modi_mount/r-packages" , .libPaths() ) )
# setwd("~/modi_mount/rsrfdev/simulate_large")
# library(mlr3)
# library(mlr3learners)
# library(mlr3extralearners)
# library(mlr3tuning)
#Make sure working directory is file directory for source files below.
#setwd("C:/Users/ricar/Documents/Universitaet-Projekte/Projekt-Paper-Chi-et-al/R-Code/rsrfdev/simulate_large")

source("util_mlr3.R")

#generates a learner given algorithm name, model_index and dimension d
#sub_index NULL => all tasks from task object will be done
#sub_index is list with values sub_index$lower, sub_index$upper => simulations only for these tasks will be done.
#...in the second case: results have to be aggregated by hand
simulate <- function(algorithm_name, model_index, d, cores = 1L, n_folds, n_evals, t_dir, r_dir, sub_index = NULL){
  simtype <- ""
  #CV Settings: n_folds, n_evals
  #LOAD TASKS
  filename <- paste0( t_dir, "/", model_index, toString(d), ".RDS")
  if( !file.exists( filename ) ){
    stop("Task file missing")
  }
  #########################
  ###RSRF nothing fixed ###
  #########################
  #fixed parameters, get setup from optimal parameter search beforehand
  if(algorithm_name == "rsrf_nf_opt"){
    simtype <- "opt"
    if( model_index == "A" && d == 4 ){
      rsrf_width_opt <- 15
      mtry_rsrf_step_opt <- 3
      mtry_cart_cart_opt <- 4
      min_node_size_opt <- 16
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE
    }
    if( model_index == "A" && d == 10 ){
      rsrf_width_opt <- 15
      mtry_rsrf_step_opt <- 9
      mtry_cart_cart_opt <- 6
      min_node_size_opt <- 10
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE
    }
    if( model_index == "A" && d == 30 ){
      rsrf_width_opt <- 30
      mtry_rsrf_step_opt <- 30
      mtry_cart_cart_opt <- 22
      min_node_size_opt <- 5
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE
    }
    if( model_index == "B" && d == 4 ){
      rsrf_width_opt <- 12
      mtry_rsrf_step_opt <- 2
      mtry_cart_cart_opt <- 4
      min_node_size_opt <- 5
      replace_opt <- FALSE
      include_CART_CART_opt <- FALSE 
    }
    if( model_index == "B" && d == 10 ){
      rsrf_width_opt <- 14
      mtry_rsrf_step_opt <- 10
      mtry_cart_cart_opt <- 7
      min_node_size_opt <- 12
      replace_opt <- FALSE
      include_CART_CART_opt <- TRUE 
    }
    if( model_index == "B" && d == 30 ){
      rsrf_width_opt <- 29
      mtry_rsrf_step_opt <- 26
      mtry_cart_cart_opt <- 23
      min_node_size_opt <- 15
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE  
    }
    if( model_index == "C" && d == 4 ){
      rsrf_width_opt <- 12
      mtry_rsrf_step_opt <- 2
      mtry_cart_cart_opt <- 3
      min_node_size_opt <- 14
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE 
    }
    if( model_index == "C" && d == 10 ){
      rsrf_width_opt <- 15
      mtry_rsrf_step_opt <- 8
      mtry_cart_cart_opt <- 2
      min_node_size_opt <- 11
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE 
    }
    if( model_index == "C" && d == 30 ){
      rsrf_width_opt <- 16
      mtry_rsrf_step_opt <- 24
      mtry_cart_cart_opt <- 24
      min_node_size_opt <- 8
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE 
    }
    if( model_index == "D" && d == 4 ){
      rsrf_width_opt <- 1
      mtry_rsrf_step_opt <- 3
      mtry_cart_cart_opt <- 2
      min_node_size_opt <- 22
      replace_opt <- FALSE
      include_CART_CART_opt <- FALSE 
    }
    if( model_index == "D" && d == 10 ){
      rsrf_width_opt <- 4
      mtry_rsrf_step_opt <- 8
      mtry_cart_cart_opt <- 3
      min_node_size_opt <- 30
      replace_opt <- FALSE
      include_CART_CART_opt <- FALSE 
    }
    if( model_index == "D" && d == 30 ){
      stop("Opt parameters not yet available")
      # mtry_rsrf_step_opt <- 
      #   mtry_cart_cart_opt <-
      #   rsrf_width_opt <-
      #   replace_opt <- 
      #   include_CART_CART_opt <-  
    }
    
    if( model_index == "E" && d == 4 ){
      rsrf_width_opt <- 13
      mtry_rsrf_step_opt <- 4
      mtry_cart_cart_opt <- 4
      min_node_size_opt <- 23
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE 
    }
    if( model_index == "E" && d == 10 ){
      rsrf_width_opt <- 15
      mtry_rsrf_step_opt <- 10
      mtry_cart_cart_opt <- 8
      min_node_size_opt <- 13
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE 
    }
    if( model_index == "E" && d == 30 ){
      rsrf_width_opt <- 25
      mtry_rsrf_step_opt <- 30
      mtry_cart_cart_opt <- 6
      min_node_size_opt <- 22
      replace_opt <- FALSE
      include_CART_CART_opt <- FALSE 
    }
    
    if( model_index == "F" && d == 6 ){
      rsrf_width_opt <- 9
      mtry_rsrf_step_opt <- 4
      mtry_cart_cart_opt <- 4
      min_node_size_opt <- 5
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE 
    }
    
      lrn <- lrn("regr.rsrf",
                mtry_mode = "nothingfixed",
                randomization = TRUE, #do not change
                test_mode = FALSE, #do not change
                saveNodeInformation = FALSE, #do not change
                rsrf_depth = 2,
                include_CART_CART = include_CART_CART_opt,
                num_trees = 100,
                min_node_size = min_node_size_opt ,
                mtry_rsrf_step = mtry_rsrf_step_opt,
                mtry_cart_cart = mtry_cart_cart_opt,
                replace = replace_opt,
                rsrf_width = rsrf_width_opt )
  }
  #CV
  if(algorithm_name == "rsrf_nf_cv"){
    simtype <- "CV"
    #SERIOUS
    lrn_to_tune = lrn("regr.rsrf",
                    mtry_mode = "nothingfixed",
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
                    rsrf_width = to_tune( 1:ifelse(d == 30, 30, 15) ), #d = 4,10: width bis 15; d=30: width bis 30.
                    include_CART_CART = to_tune( c(TRUE,FALSE) )
      )
      lrn <- auto_tuner(
        tnr("random_search", batch_size = 10), #Batch size 10 and 10 folds means that 100 cores can be used in parallel
        learner = lrn_to_tune,
        resampling = rsmp("cv", folds = n_folds),
        measure = msr("regr.mse"),
        terminator = trm("evals", n_evals = n_evals)
      )
  }

  
  ####################
  ###RSRF allfixed ###
  ####################
  #fixed parameters
  if(algorithm_name == "rsrf_af_opt"){
    simtype <- "opt"
    if( model_index == "A" && d == 4 ){
        rsrf_width_opt <- 8
        mtry_rsrf_step_opt <- 4
        mtry_rsrf_step_random_opt <- 3
        min_node_size_opt <- 14
        replace_opt <- TRUE
        include_CART_CART_opt <- FALSE
    }
    if( model_index == "A" && d == 10 ){
      rsrf_width_opt <- 12
      mtry_rsrf_step_opt <- 8
      mtry_rsrf_step_random_opt <- 9
      min_node_size_opt <- 5
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE
    }
    if( model_index == "A" && d == 30 ){
      rsrf_width_opt <- 28
      mtry_rsrf_step_opt <- 26
      mtry_rsrf_step_random_opt <- 26
      min_node_size_opt <- 6
      replace_opt <- FALSE
      include_CART_CART_opt <- TRUE
    }
    if( model_index == "B" && d == 4 ){
      rsrf_width_opt <- 11
      mtry_rsrf_step_opt <- 3
      mtry_rsrf_step_random_opt <- 4
      min_node_size_opt <- 10
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE
    }
    if( model_index == "B" && d == 10 ){
      rsrf_width_opt <- 14
      mtry_rsrf_step_opt <- 9
      mtry_rsrf_step_random_opt <- 8
      min_node_size_opt <- 11
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE
    }
    if( model_index == "B" && d == 30 ){
      rsrf_width_opt <- 24
      mtry_rsrf_step_opt <- 30
      mtry_rsrf_step_random_opt <- 19
      min_node_size_opt <- 17
      replace_opt <- FALSE
      include_CART_CART_opt <- TRUE
    }
    if( model_index == "C" && d == 4 ){
      rsrf_width_opt <- 14
      mtry_rsrf_step_opt <- 2
      mtry_rsrf_step_random_opt <- 4
      min_node_size_opt <- 12
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE
    }
    if( model_index == "C" && d == 10 ){
      rsrf_width_opt <- 12
      mtry_rsrf_step_opt <- 10
      mtry_rsrf_step_random_opt <- 9
      min_node_size_opt <- 7
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE
    }
    if( model_index == "C" && d == 30 ){
      rsrf_width_opt <- 12
      mtry_rsrf_step_opt <- 26
      mtry_rsrf_step_random_opt <- 22
      min_node_size_opt <- 30
      replace_opt <- TRUE
      include_CART_CART_opt <- TRUE
    }
    if( model_index == "D" && d == 4 ){
      rsrf_width_opt <- 2
      mtry_rsrf_step_opt <- 3
      mtry_rsrf_step_random_opt <- 1
      min_node_size_opt <- 23
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE
    }
    if( model_index == "D" && d == 10 ){
      rsrf_width_opt <- 1
      mtry_rsrf_step_opt <- 10
      mtry_rsrf_step_random_opt <- 5
      min_node_size_opt <- 30
      replace_opt <- FALSE
      include_CART_CART_opt <- FALSE
    }
    if( model_index == "D" && d == 30 ){
      stop("Opt parameters not yet available")
      # rsrf_width_opt <- 12
      # mtry_rsrf_step_opt <- 8
      # mtry_rsrf_step_random_opt <- 9
      # min_node_size_opt <- 5
      # replace_opt <- TRUE
      # include_CART_CART_opt <- TRUE
    }
    
    if( model_index == "E" && d == 4 ){
      rsrf_width_opt <- 3
      mtry_rsrf_step_opt <- 2
      mtry_rsrf_step_random_opt <- 4
      min_node_size_opt <- 20
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE
    }
    
    if( model_index == "E" && d == 10 ){
      rsrf_width_opt <- 13
      mtry_rsrf_step_opt <- 10
      mtry_rsrf_step_random_opt <- 8
      min_node_size_opt <- 13
      replace_opt <- FALSE
      include_CART_CART_opt <- FALSE
    }
    
    if( model_index == "E" && d == 30 ){
      rsrf_width_opt <- 24
      mtry_rsrf_step_opt <- 28
      mtry_rsrf_step_random_opt <- 24
      min_node_size_opt <- 29
      replace_opt <- TRUE
      include_CART_CART_opt <- FALSE
    }
    
    if( model_index == "F" && d == 6 ){
      rsrf_width_opt <- 15
      mtry_rsrf_step_opt <- 4
      mtry_rsrf_step_random_opt <- 5
      min_node_size_opt <- 9
      replace_opt <- FALSE
      include_CART_CART_opt <- FALSE
    }
    
    lrn <- lrn("regr.rsrf",
               mtry_mode = "allfixed",
               mtry_cart_cart = NULL, #NULL when allfixed
               fixed_cart_cart = TRUE, #TRUE when allfixed (will be ignored if include_CART_CART is FALSE)
               randomization = TRUE, #do not change
               test_mode = FALSE, #do not change
               saveNodeInformation = FALSE, #do not change
               num_trees = 100,
               rsrf_depth = 2,
               #optimal parameters from findOpt
               rsrf_width = rsrf_width_opt,
               include_CART_CART = include_CART_CART_opt,
               min_node_size = min_node_size_opt ,
               mtry_rsrf_step_random = mtry_rsrf_step_random_opt,
               mtry_rsrf_step = mtry_rsrf_step_opt,
               replace = replace_opt
               )
  }
  #CV
  if(algorithm_name == "rsrf_af_cv"){
    simtype <- "CV"
    lrn_to_tune = lrn("regr.rsrf",
                  mtry_mode = "allfixed",
                  fixed_cart_cart = TRUE, #TRUE when allfixed (will be ignored if include_CART_CART is FALSE)
                  randomization = TRUE, #do not change
                  test_mode = FALSE, #do not change
                  saveNodeInformation = FALSE, #do not change
                  rsrf_depth = 2,
                  include_CART_CART = TRUE,
                  num_trees = 100,
                  min_node_size = to_tune( 5:30 ),
                  mtry_rsrf_step_random = to_tune( 1:d),
                  mtry_rsrf_step = to_tune( 1:d ),
                  replace = to_tune( c(TRUE, FALSE )),
                  rsrf_width = to_tune( 1:ifelse(d == 30, 30, 15) ), #d = 4,10: width bis 15; d=30: width bis 30.
                  
    )
    lrn <- auto_tuner(
      tnr("random_search", batch_size = 10), #Batch size 10 and 10 folds means that 100 cores can be used in parallel
      resampling = rsmp("cv", folds = n_folds),
      measure = msr("regr.mse"),
      terminator = trm("evals", n_evals = n_evals)
    )
  }
    
    #########################
    ###INTERACTION FORESTS###
    #########################
    #fixed parameters
  if( algorithm_name == "intf_opt"){
    simtype <- "opt"  
    
    #npairs range from 1 to 100  
    if( model_index == "A" && d == 4 ){
      num.trees <- 500
      min.node.size <- 20
      replace <- TRUE
      npairs <- 14
    }
    
    # old results when npairs ranges from 1 to 15  
    # if( model_index == "A" && d == 4 ){
    #     num.trees <- 500
    #     min.node.size <- 20
    #     replace <- TRUE
    #     npairs <- 15
    # }
    # 
    
    
    
    # old results when npairs ranges from 1 to 30  
    # if( model_index == "A" && d == 10 ){
    #     num.trees <- 500
    #     min.node.size <- 5
    #     replace <- FALSE
    #     npairs <- 28
    # }
    
    #npairs range from 1 to 250  
    if( model_index == "A" && d == 10 ){
        num.trees <- 500
        min.node.size <- 11
        replace <- FALSE
        npairs <- 153
    }
    
    # old results when npairs ranges from 1 to 30  
    # if( model_index == "A" && d == 30 ){
    #   num.trees <- 500
    #   min.node.size <- 10
    #   replace <- FALSE
    #   npairs <- 30
    # }
    
    #npairs range from 1 to 750  
    if( model_index == "A" && d == 30 ){
        num.trees <- 500
        min.node.size <- 11
        replace <- FALSE
        npairs <- 749
    }
    
    #npairs ranges from 1 to 100
    if( model_index == "B" && d == 4 ){
        num.trees <- 500
        min.node.size <- 10
        replace <- FALSE
        npairs <- 7
    }
    
    # old results when npairs ranges from 1 to 10  
    # if( model_index == "B" && d == 4 ){
    #   num.trees <- 500
    #   min.node.size <- 7
    #   replace <- FALSE
    #   npairs <- 4
    # }
    
    # old results when npairs ranges from 1 to 30  
    # if( model_index == "B" && d == 10 ){
    #     num.trees <- 500
    #     min.node.size <- 7
    #     replace <- FALSE
    #     npairs <- 29
    # }
    
    #npairs range from 1 to 250  
    if( model_index == "B" && d == 10 ){
      num.trees <- 500
      min.node.size <- 8
      replace <- FALSE
      npairs <- 110
    }
      
    # old results when npairs ranges from 1 to 30  
    # if( model_index == "B" && d == 30 ){
    #     num.trees <- 500
    #     min.node.size <- 8
    #     replace <- FALSE
    #     npairs <- 29
    # }
    # 
    
    #npairs range from 1 to 750  
    if( model_index == "B" && d == 30 ){
      num.trees <- 500
      min.node.size <- 17
      replace <- FALSE
      npairs <- 450
    }
    
    #npairs range from 1 to 100  
    if( model_index == "C" && d == 4 ){
      num.trees <- 500
      min.node.size <- 13
      replace <- TRUE
      npairs <- 23
    }
    
    # old results when npairs ranges from 1 to 15  
    # if( model_index == "C" && d == 4 ){
    #     num.trees <- 500
    #     min.node.size <- 7
    #     replace <- FALSE
    #     npairs <- 6
    # }
    
    # old results when npairs ranges from 1 to 30  
    # if( model_index == "C" && d == 10 ){
    #     num.trees <- 500
    #     min.node.size <- 6
    #     replace <- FALSE
    #     npairs <- 24
    # }
    
    #npairs range from 1 to 250  
    if( model_index == "C" && d == 10 ){
      num.trees <- 500
      min.node.size <- 14
      replace <- FALSE
      npairs <- 33
    }
    
    # old results when npairs ranges from 1 to 30  
    #if( model_index == "C" && d == 30 ){
    #    num.trees <- 500
    #    min.node.size <- 10
    #    replace <- FALSE
    #    npairs <- 29
    #}
      
    #npairs ranges from 1 to 750  
    if( model_index == "C" && d == 30 ){
        num.trees <- 500
        min.node.size <- 18
        replace <- FALSE
        npairs <- 99
    }
    
    if( model_index == "D" && d == 4 ){
        stop("Opt parameters not yet available")
        num.trees <- 500
        min.node.size <- NA
        replace <- NA
        npairs <- NA
    }
      
    if( model_index == "D" && d == 10 ){
        stop("Opt parameters not yet available")
        num.trees <- 500
        min.node.size <- NA
        replace <- NA
        npairs <- NA
    }
      
    if( model_index == "D" && d == 30 ){
        stop("Opt parameters not yet available")
        num.trees <- 500
        min.node.size <- NA
        replace <- NA
        npairs <- NA
    }
    
    if( model_index == "E" && d == 4 ){
      num.trees <- 500
      min.node.size <- 16
      replace <- FALSE
      npairs <- 2
    }
    
    if( model_index == "E" && d == 10 ){
      num.trees <- 500
      min.node.size <- 26
      replace <- FALSE
      npairs <- 151
    }
    
    if( model_index == "E" && d == 30 ){
      num.trees <- 500
      min.node.size <- 28
      replace <- FALSE
      npairs <- 30
    }
    
    if( model_index == "F" && d == 6 ){
      num.trees <- 500
      min.node.size <- 22
      replace <- TRUE
      npairs <- 99
    }
      
    lrn = lrn("regr.intf",    num.trees = num.trees, 
                              min.node.size = min.node.size, 
                              replace = replace,
                              npairs = npairs)
  }
  #CV
  if(algorithm_name == "intf_cv"){
      simtype<-"CV"
      lrn_to_tune = lrn("regr.intf",
                        num.trees = 500, 
                        min.node.size = to_tune(5:30), 
                        replace = to_tune( c(TRUE, FALSE )),
                        npairs = to_tune( 1 : ifelse(d==4, 15, 30) ) #OLD setting!!!
      )
      lrn <- auto_tuner(
        tnr("random_search"),
        learner = lrn_to_tune,
        resampling = rsmp("cv", folds = n_folds),
        measure = msr("regr.mse"),
        terminator = trm("evals", n_evals = n_evals)
      )
    }
  ############
  ###RANGER###
  ############
  #fixed parameters
  if( algorithm_name == "ranger_opt"){
    simtype <- "opt"
    
    if( model_index == "A" && d == 4 ){
      num.trees <- 500
      min.node.size <- 5
      replace <- TRUE
      mtry <- 4
    }
    
    if( model_index == "A" && d == 10 ){
      num.trees <- 500
      min.node.size <- 5
      replace <- FALSE
      mtry <- 10
    }
    
    if( model_index == "A" && d == 30 ){
      num.trees <- 500
      min.node.size <- 7
      replace <- FALSE
      mtry <- 30
    }
    
    if( model_index == "B" && d == 4 ){
      num.trees <- 500
      min.node.size <- 8
      replace <- TRUE
      mtry <- 3
    }
    
    if( model_index == "B" && d == 10 ){
      num.trees <- 500
      min.node.size <- 6
      replace <- TRUE
      mtry <- 9
    }
    
    if( model_index == "B" && d == 30 ){
      num.trees <- 500
      min.node.size <- 12
      replace <- TRUE
      mtry <- 28
    }
    
    if( model_index == "C" && d == 4 ){
      num.trees <- 500
      min.node.size <- 5
      replace <- TRUE
      mtry <- 2
    }
    
    if( model_index == "C" && d == 10 ){
      num.trees <- 500
      min.node.size <- 15
      replace <- TRUE
      mtry <- 7
    }
    
    if( model_index == "C" && d == 30 ){
      num.trees <- 500
      min.node.size <- 18
      replace <- TRUE
      mtry <- 26
    }
    
    if( model_index == "D" && d == 4 ){
      num.trees <- 500
      min.node.size <- 28
      replace <- TRUE
      mtry <- 2
    }
    
    if( model_index == "D" && d == 10 ){
      num.trees <- 500
      min.node.size <- 27
      replace <- TRUE
      mtry <- 5
    }
    
    if( model_index == "D" && d == 30 ){
      num.trees <- 500
      min.node.size <- 30
      replace <- TRUE
      mtry <- 18
    }
    
    if( model_index == "E" && d == 4 ){
      num.trees <- 500
      min.node.size <- 10
      replace <- TRUE
      mtry <- 2
    }
    
    if( model_index == "E" && d == 10 ){
      num.trees <- 500
      min.node.size <- 8
      replace <- TRUE
      mtry <- 5
    }
    
    if( model_index == "E" && d == 30 ){
      num.trees <- 500
      min.node.size <- 30
      replace <- TRUE
      mtry <- 20
    }
    
    if( model_index == "F" && d == 6 ){
      num.trees <- 500
      min.node.size <- 6
      replace <- TRUE
      mtry <- 5
    }
    
    
    lrn = lrn("regr.ranger",
                num.trees = num.trees, 
                min.node.size = min.node.size, 
                replace = replace,
                mtry = mtry
              )
  }
  #CV
  if( algorithm_name == "ranger_cv"){
    simtype <- "CV"
    lrn_to_tune = lrn("regr.ranger",
                      num.trees = 500, 
                      min.node.size = to_tune( 5:30 ), 
                      replace = to_tune( c(TRUE,FALSE) ),
                      mtry = to_tune(1:d)
    )
    lrn <- auto_tuner(
      tnr("random_search"),
      learner = lrn_to_tune,
      resampling = rsmp("cv", folds = n_folds),
      measure = msr("regr.mse"),
      terminator = trm("evals", n_evals = n_evals)
    )
  }  
  
  ############
  ###Ranger (Extratrees)###
  ############
  #fixed parameters
  if( algorithm_name == "extratrees_opt"){
    simtype <- "opt"
    if( model_index == "A" && d == 4 ){
      mtry  <- 4
      min.node.size <- 12
      replace <- FALSE
      num.random.splits <- 3
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "A" && d == 10 ){
      mtry <- 9
      min.node.size <- 5
      replace <- FALSE
      num.random.splits <- 3
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "A" && d == 30 ){
      mtry <- 29
      min.node.size <- 5
      replace <- FALSE
      num.random.splits <- 6
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "B" && d == 4 ){
      mtry <- 3
      min.node.size <- 8
      replace <- FALSE
      num.random.splits <- 3
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "B" && d == 10 ){
      mtry <- 9
      min.node.size <- 5
      replace <- FALSE
      num.random.splits <- 3
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "B" && d == 30 ){
      mtry <- 29
      min.node.size <- 9
      replace <- FALSE
      num.random.splits <- 2
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "C" && d == 4 ){
      mtry <- 3
      min.node.size <- 6
      replace <- TRUE
      num.random.splits <- 5
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "C" && d == 10 ){
      mtry <- 7
      min.node.size <- 10
      replace <- FALSE
      num.random.splits <- 3
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "C" && d == 30 ){
      mtry <- 29
      min.node.size <- 16
      replace <- FALSE
      num.random.splits <- 3
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "D" && d == 4 ){
      stop("Opt parameters not yet available")
      
    }
    
    if( model_index == "D" && d == 10 ){
      stop("Opt parameters not yet available")
      
    }
    
    if( model_index == "D" && d == 30 ){
      stop("Opt parameters not yet available")
      
    }
    
    if( model_index == "E" && d == 4 ){
      mtry <- 2
      min.node.size <- 10
      replace <- FALSE
      num.random.splits <- 1
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "E" && d == 10 ){
      mtry <- 7
      min.node.size <- 6
      replace <- TRUE
      num.random.splits <- 1
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1

    }
    
    if( model_index == "E" && d == 30 ){
      mtry <- 28
      min.node.size <- 15
      replace <- TRUE
      num.random.splits <- 1
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    if( model_index == "F" && d == 6 ){
      mtry <- 1
      min.node.size <- 5
      replace <- FALSE
      num.random.splits <- 5
      num.trees <- 500
      splitrule <- "extratrees"
      sample.fraction <- 1
    }
    
    #extratrees
    lrn = lrn("regr.ranger",
              num.trees = num.trees, 
              min.node.size = min.node.size, 
              num.random.splits = num.random.splits,
              replace = replace,
              mtry = mtry,
              sample.fraction = sample.fraction,
              splitrule = splitrule
    )
  }
  #CV
  if( algorithm_name == "extratrees_cv"){
    simtype <- "CV"
    lrn_to_tune = lrn("regr.ranger",
                      mtry = to_tune( 1:d ),
                      num.trees = 500,
                      min.node.size = to_tune( 5:30 ),
                      replace = to_tune( c(TRUE,FALSE)),
                      num.random.splits = to_tune( 1:10 ),
                      splitrule = "extratrees",
                      sample.fraction = 1 #Note: for replace == FALSE, this choice makes it the original extremely random forest 
                      
    )
    lrn <- auto_tuner(
      tnr("random_search"),
      learner = lrn_to_tune,
      resampling = rsmp("cv", folds = n_folds),
      measure = msr("regr.mse"),
      terminator = trm("evals", n_evals = n_evals)
    )
  }  
  
  
  ######### READ files #######  

  tlist <- readRDS( filename )
  if( !all.equal( c(model_index, d), c( tlist$model_index, tlist$d ) ) ){
    stop("Task file and simulation request not compatible")
  }
  
  #Save tuning setup in case of CV
  if( simtype == "CV"){
    ret_tuning_setup <- as.data.table( lrn$param_set$clone(deep=TRUE)$get_values()  )
    print(ret_tuning_setup)
    ret <- NA
  }
  #
  
  if(is.null(sub_index)){
  #Get predictions. Computes in parallel if desired.
  #Parallel using mclapply
  #error <- parallel::mclapply( tlist$tasks,  mlr3simulateFromLearner, learner=lrn, mc.cores = cores )
  
  ###Parallel using future and internal parallelization from mlr3tuning 
  future::plan(strategy="multicore", workers = cores )
  error <- lapply( tlist$tasks,  mlr3simulateFromLearner, learner=lrn )
  ###
  }else{
    if( sub_index$lower < 1 || length( tlist$tasks) < sub_index$upper ){
      stop("Error. sub_index specifications not compatible with tasks list.")
    }
    future::plan(strategy="multicore", workers = cores )
    error <- lapply( tlist$tasks[sub_index$lower : sub_index$upper],  mlr3simulateFromLearner, learner=lrn )
  }
  if(simtype == "") ret_tuning_setup <- ret <- "not available as simtype was not set"
  if( simtype == "opt"){
    ret <- lrn$param_set$get_values()
    ret_tuning_setup <- NA
  }
  return( list( simplify2array( error ), ret, simtype, ret_tuning_setup ) ) 
  #Returns list containing errors, parameters chosen (NULL if CV), and simtype "opt" or "CV"
}

#############
####START####
#############
print("START. setup_dt: ")
setup_dt <- readRDS( paste0(tasks_dir, "/setup_dt.RDS") )
print(setup_dt)

print( paste("IDs chosen: ", paste(sim_IDs, collapse= ", ")) )

########################
#Create result_dt only for these rows
result_dt <- setup_dt[ID %in% sim_IDs]
#Possibly save sub_index in result_dt_object
if(!is.null(sim_sub_index)){
  result_dt[,subset_lower := sim_sub_index$lower ]
  result_dt[,subset_upper := sim_sub_index$upper ]
}
#SET OUTPUT FILE NAMES
if(is.null(sim_sub_index)){
  #Create result txt file
  result_txt <- paste0( results_dir, "/result_txt_", paste(sim_IDs, collapse= "_"), ".txt" )
  #Create result rds filename
  result_rds_filename <- paste0( paste0( results_dir, "/result_dt_", paste(sim_IDs, collapse= "_") ), ".RDS" )
}else{
  sub_index_txt <- paste0( "_subset_task_from_", sim_sub_index$lower, "_to_", sim_sub_index$upper )
  result_txt <- paste0( results_dir, "/result_txt_", paste(sim_IDs, collapse= "_"), sub_index_txt, ".txt" )
  result_rds_filename <- paste0( paste0( results_dir, "/result_dt_", paste(sim_IDs, collapse= "_") ), sub_index_txt, ".RDS" )
}
###
cat( "RESULTS\n\n",file =result_txt )
cat( "CV setting (for the CV algorithms)\n",
     "no. randomly chosen evals:", sim_n_evals, "\n",
     "no. folds                :", sim_n_folds, "\n\n", file=result_txt, append=TRUE)
cat("-------------------------------------------------\n",
    "       FULL SETTING  (extracted from file)       \n",
    "-------------------------------------------------\n", file=result_txt, append=TRUE)
write.table(setup_dt, sep = "****", append=TRUE, file=result_txt, row.names = FALSE)
cat("-------------------------------------------------\n",
    "           SUBSET FOR THIS SIMULATION            \n",
    "-------------------------------------------------\n\n", file=result_txt, append=TRUE)
cat( paste("IDs chosen: ", paste(sim_IDs, collapse= ", ")), "\n\n", file=result_txt, append=TRUE )
cat( "Table: \n\n", file=result_txt, append=TRUE)
write.table(result_dt, sep = "****", append=TRUE, file=result_txt, row.names = FALSE)
if(!is.null(sim_sub_index)){
  cat( paste("ATTENTION: Subsetting data.", "\n",
             "lower index: ", sim_sub_index$lower, "\n",
             "upper index: ", sim_sub_index$upper, "\n"),append=TRUE, file=result_txt) 
}


cat("-------------------------------------------------\n",
    "                     Seed used                   \n",
    "-------------------------------------------------\n\n",
    ifelse( is.null(seed_used), "No seed used", paste0("Seed: ", seed_used) ), "\n\n", file=result_txt,append=TRUE)

cat("-------------------------------------------------\n",
    "         PROGRESS and RESULTS for each ID        \n",
    "-------------------------------------------------\n\n", file=result_txt, append=TRUE)
for( id in sim_IDs ){
  cat(paste0( "Simulating for ID:", id), "\n\n", file=result_txt, append=TRUE)
  
  
  print( setup_dt[ID == id] )
  start_time <- Sys.time()
  cat("Start time:        ", paste(start_time), "\n", file=result_txt, append=TRUE)
  fromSimulate <- simulate( setup_dt[ID == id]$algorithm,
                      setup_dt[ID == id]$model,
                      d = setup_dt[ID == id]$d,
                      cores = sim_cores,
                      n_folds = sim_n_folds,
                      n_evals = sim_n_evals,
                      t_dir = tasks_dir,
                      r_dir = results_dir,
                      sub_index = sim_sub_index)
  errors <- fromSimulate[[1]]
  param_values <- fromSimulate[[2]]
  simtype <- fromSimulate[[3]]
  param_tune_setting <- fromSimulate[[4]] #In case of CV this is a data.table containing the tuning setting
  
  end_time <- Sys.time()
  diff_time <- difftime(end_time, start_time )
  print(paste( "Finished simulation for current id.",
               "Time difference was:",diff_time,attr(diff_time,"units") ) )
  cat("End time:           ", paste(end_time), "\n", file=result_txt, append=TRUE)
  cat("Finished simulation for current id.\n",
      "Time difference was:",diff_time,attr(diff_time,"units"),"\n\n", file=result_txt, append=TRUE )
  
  
  if( simtype == "CV"){
    result_dt[ID == id, param_values := param_values]
    cat("     -------------------------------------------------\n",
        "               TUNING SETUP (for current ID)          \n",
        "     -------------------------------------------------\n\n", file=result_txt, append=TRUE)
    write.table(param_tune_setting, sep = "****", append=TRUE, file=result_txt, row.names = FALSE)
  }
  cat("     -------------------------------------------------\n",
      "             Result for current ID=", id, ":\n",
      "     -------------------------------------------------\n", file=result_txt, append=TRUE)
  result_dt[ID == id, errorlist := list(errors) ]
  result_dt[ID == id, error := mean( errorlist[[1]] ) ]
  result_dt[ID == id, sd := sd( errorlist[[1]] ) ]
  result_dt[ID == id, simtype := simtype]
  if( simtype == "opt"){
    result_dt[ID == id, param_values := list(param_values) ]
    #Print parameters to result file
    cat(file=result_txt, append=TRUE, "Parameters used for this simulations (these were set by hand).")
    for( i in 1:length(param_values)){
      st <- names(param_values)
      cat( file=result_txt, 
           st[i], "____________", toString( param_values[st[i]][[1]] ),"\n\n", append=TRUE)
    }
  }

  write.table(result_dt[ID==id,.SD, .SDcols = !c("errorlist", "param_values")], sep = "****", append=TRUE, file=result_txt, row.names = FALSE)
  cat("\n-------------------------------------------------\n", file=result_txt, append=TRUE)
}


#WRITE SUMMARY OF FULL RESULT INTO RESULT FILE.
#IF DATA WAS SUBSETTED, ONLY A WARNING MESSAGE IS REPORTED AS REMINDER
if(is.null(sim_sub_index)){
cat("-------------------------------------------------\n",
    "                ALL RESULTS in one table         \n",
    "-------------------------------------------------\n\n", file=result_txt, append=TRUE)

# Prints result (list of errors is saved in cells of column errorlist though not printed)
print( result_dt[,.SD, .SDcols = !c("errorlist", "param_values")] )
write.table(result_dt[,.SD, .SDcols =!c("errorlist", "param_values")], sep = "****", append=TRUE, file=result_txt, row.names = FALSE)
}else{
  cat("-------------------------------------------------\n",
      "                  ATTENTION                      \n",
      "-------------------------------------------------\n\n", file=result_txt, append=TRUE)
  
  cat("Simulations have only been done for tasks from tasklist for indices...", "\n", 
      "lower index: ", sim_sub_index$lower, "\n",
      "upper index: ", sim_sub_index$upper, "\n",
      "Results are available in file:", result_rds_filename, "\n \n",
      "Make sure to aggregate results to the result for the whole data set manually!", "\n\n",
      "NOTE: Errors and standard deviations reported above only refer to \n the simulations made on the task subset with the specifed indices.",
      file=result_txt, append=TRUE)
}

saveRDS( result_dt, file= result_rds_filename)
print("DONE.")