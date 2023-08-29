##' @title Tree class
##' @description Virtual class for Random forest tree.
##' Contains all fields and methods used in all tree subclasses.
Tree <- setRefClass("Tree",
  fields = list(
    min_node_size = "integer",
    splitrule = "character",
    data = "Data",
    sampleIDs = "list",
    oob_sampleIDs = "integer",
    child_nodeIDs = "list",
    split_varIDs = "integer",
    split_values = "numeric", #for each nodeID, saves the split value. If terminal node: saves estimation value
    split_random_or_not = "logical", #Not necessary #To save for every node whether random split has been made or not
    current_terminal_nodeIDs = "ANY",
    current_terminal_nodeIDs_tryout = "ANY",
    current_varIDs_random = "ANY",
    current_varIDs_cart = "ANY",
    use_all_data = "logical",
    randomization_step = "logical",
    rsrf_width = "integer",
    rsrf_depth = "integer",
    mtry_mode = "character",
    mtry_rsrf_step = "integer",
    mtry_rsrf_step_random = "integer",
    mtry_cart_cart = "integer",
    fixed_cart_cart = "logical",
    include_CART_CART = "logical", 
    test_mode = "logical",
    saveNodeInformation = "logical",
    node_information = "ANY"),
  methods = list(

    grow = function(replace, use_all_data = FALSE, rsrf_width) {
      ##Possibly initialize node_information
      if( saveNodeInformation == TRUE){
        node_information <<- list()
      }
      
      ## Bootstrap
      num_samples <- data$nrow
      if( use_all_data ){ #Use all samples: No bootstrap regardless of value for replace
        sampleIDs <<- list(1:num_samples)
        oob_sampleIDs <<- integer(0)
      } else{
        if (replace) {
          num_bootstrap_samples <- num_samples
        } else {
          num_bootstrap_samples <- num_samples * 0.6321
        }
        bootstrap_sample <- sample(num_samples, num_bootstrap_samples, replace = replace)
        oob_sampleIDs <<- (1:num_samples)[-bootstrap_sample]
        ## Assign bootstrap samples to root node
        sampleIDs <<- list(bootstrap_sample)
      }
      ## Call recursive splitting function on root
      if( randomization_step == FALSE ){
        #In this case a tree is built as in simpleRF. mtry_cart_cart takes role of mtry
       splitNode(1, tryout = FALSE)
      }else{
        if ( !(include_CART_CART == FALSE && rsrf_width == 0 ) ){
          #In this case recursive tree building is made by placing tryout-splits at each terminal nodes.
          #If rsrf_width == 0 
          #   ->this is RF tree with mtry_cart_cart taking role of mtry
          #If rsrf_width == 1 and include_CART_CART == TRUE
          #   ->this is RF tree with mtry_rsrf_step taking role of mtry
          current_terminal_nodeIDs <<- 1
          splitNodeWithTryout() #Call recursive splitting function which makes tryout - rounds
        }else{
          stop("Combination of parameters is not possible.")
        }
      }
    },

    splitNodeWithTryout = function(){
      for( node in current_terminal_nodeIDs){
         if(saveNodeInformation == TRUE){
           node_information[[node]] <<- list()
         }
         current_terminal_nodeIDs <<- NULL
         splitTryout(node, rsrf_width )
         splitNodeWithTryout()
      }
    },

    splitNode = function(nodeID, current_tree_depth = 0, tryout = FALSE ) {
      if(tryout == TRUE && current_tree_depth == rsrf_depth){
        current_terminal_nodeIDs <<- append( current_terminal_nodeIDs, nodeID )
        current_terminal_nodeIDs_tryout <<- append( current_terminal_nodeIDs_tryout, nodeID )
        return( invisible(NULL) )
      }

      ## Split node (possibly) at random
      if( randomization_step == TRUE){#With Randomization
        if( (current_tree_depth + 1) %% rsrf_depth == 0 ){#CART
          if( tryout == FALSE || is.null(current_varIDs_cart)){
            #Sample possible split variables, Use mtry for Random-CART step here
            possible_split_varIDs <- sample(data$ncol-1, mtry_rsrf_step, replace = FALSE) + 1
          }else{
            #Use fixed varIDs from list
            possible_split_varIDs <- current_varIDs_cart[[1]] 
            # print( paste("...Daughter NodeID: ", nodeID, "uses candidates", toString( current_varIDs_cart[[1]])))
            if( saveNodeInformation == TRUE){
              node_information[[nodeID]] <<- list()
              node_information[[nodeID]]$candidate_varIDs_cart_used <<- current_varIDs_cart[[1]]
            }
            current_varIDs_cart <<- current_varIDs_cart[-1] #Remove first element

          }
          split_random_or_not[[nodeID]] <<- FALSE
          split <- splitNodeInternal(nodeID, possible_split_varIDs)
        }else{#Random
          ### put following lines as comment or delete when removing "test_mode" 
          split_random_or_not[[nodeID]] <<- TRUE
          if(test_mode == TRUE){
            #current_varIDs_random is either NULL or list containing vectors of varIDs. Note that NULL[[1]] is NULL.
            split <- splitNodeInternalRandom(nodeID, possible_split_varIDs = current_varIDs_random[[1]] )
          }else{
            #current_varIDs_random is either NULL or list containing vectors of varIDs. Note that NULL[[1]] is NULL.
            split <- splitNodeInternalRandom(nodeID, possible_split_varIDs = current_varIDs_random[[1]] )
          }
          ### ...until here. Uncomment following line when removing "test_mode"
          ##current_varIDs_random is either NULL or list containing vectors of varIDs. Note that NULL[[1]] is NULL.
          #split <- splitNodeInternalRandom(nodeID, possible_split_varIDs = current_varIDs_random[[1]] )
        }
      }else{
        #Without Randomization
        if( fixed_cart_cart == TRUE){#Only in mtry modes allfixed or semifixed
          if( mtry_mode == "allfixed"){
            if( (current_tree_depth + 1) %% rsrf_depth == 0 ){
              possible_split_varIDs <- current_varIDs_cart[[1]]
              current_varIDs_cart <<- current_varIDs_cart[-1] #Remove first element
            }else{
              possible_split_varIDs <- current_varIDs_random[[1]]
            }
          }
          if( mtry_mode == "semifixed"){
            if( (current_tree_depth + 1) %% rsrf_depth == 0 ){
              possible_split_varIDs <- sample(data$ncol-1, mtry_cart_cart, replace = FALSE) + 1
            }else{
              possible_split_varIDs <- current_varIDs_random[[1]]
            }
          }
          split <- splitNodeInternal(nodeID, possible_split_varIDs)
          split_random_or_not[[nodeID]] <<- FALSE
        }else{
        ## Sample possible split variables; Use mtry for CART-CART here
        possible_split_varIDs <- sample(data$ncol-1, mtry_cart_cart, replace = FALSE) + 1
        split_random_or_not[[nodeID]] <<- FALSE
        split <- splitNodeInternal(nodeID, possible_split_varIDs)
        }
      }

      if (!is.null(split)) {
        ## Assign split
        split_varIDs[[nodeID]] <<- split$varID
        split_values[[nodeID]] <<- split$value

        ## Create child nodes
        left_child <- length(sampleIDs) + 1
        right_child <- length(sampleIDs) + 2
        child_nodeIDs[[nodeID]] <<- c(left_child, right_child)
        # print( paste("nodeID: ", nodeID, " has child nodes ", toString( child_nodeIDs[[nodeID]] )  ))
        # print( current_varIDs_cart )
        idx <- data$subset(sampleIDs[[nodeID]], split$varID) <= split$value
        sampleIDs[[left_child]] <<- sampleIDs[[nodeID]][idx]
        sampleIDs[[right_child]] <<- sampleIDs[[nodeID]][!idx]
        #OUTCOMMENTED: OLD CODE FROM SEARCHING AN ERROR OCCURING OCASSIONALLY
        # if( length(sampleIDs[[nodeID]][idx]) == 0 ){
        #   stop("No sample IDs in child node")
        # }
        # if( length(sampleIDs[[nodeID]][!idx]) == 0 ){
        #   stop("No sample IDs in child node")
        # }
        #END
        
        ## Recursively call split node on child nodes
        current_tree_depth <- current_tree_depth + 1
        if( tryout == TRUE){
        splitNode(left_child, current_tree_depth, tryout = TRUE)
        splitNode(right_child, current_tree_depth, tryout = TRUE)
        } else{
          splitNode(left_child, current_tree_depth, tryout = FALSE)
          splitNode(right_child, current_tree_depth, tryout = FALSE)
        }
      } else {
        ## Compute estimate for terminal node
        split_values[[nodeID]] <<- estimate(nodeID)
        split_varIDs[[nodeID]] <<- NA
        # If in tryout mode, add such a node to the tryout terminal node ID list (because when calculate the impurity decrease after 2 or more steps, all these nodes are needed)
        if( tryout == TRUE){
          current_terminal_nodeIDs_tryout <<- append( current_terminal_nodeIDs_tryout, nodeID )
        }
      }
    },


    splitTryout = function( nodeID, rsrf_width ){
      #Save all global variables that will be changed when growing a tree
      split_values_tryout_start <- split_values
      split_varIDs_tryout_start <- split_varIDs
      split_random_or_not_tryout_start <- split_random_or_not
      child_nodeIDs_tryout_start <- child_nodeIDs
      sampleIDs_start <- sampleIDs
      best_decrease <- -1
      update_made <- FALSE
      if(include_CART_CART == TRUE){
        loop_start <- 0
      }else{
        loop_start <- 1
      }
      if(mtry_mode == "nothingfixed"){
        current_varIDs_random <<- NULL
        current_varIDs_cart <<- NULL
      }
      if( mtry_mode == "semifixed" ){ #varIDs will be fixed over all RSRF-Step Candidate Splits for the random split only
        current_varIDs_random <<- list( sample( data$ncol - 1, mtry_rsrf_step_random ) + 1  )
        current_varIDs_cart <<- NULL
      }
      if( mtry_mode == "allfixed"){ #varIDs will be fixed over all RSRF-Step Candidate Splits for the random and the two CART splits
        current_varIDs_random <<- list( sample( data$ncol - 1, mtry_rsrf_step_random ) + 1  )
        current_varIDs_cart <<- list()
        current_varIDs_cart[[1]] <<- sample( data$ncol - 1, mtry_rsrf_step ) + 1 
        current_varIDs_cart[[2]] <<- sample( data$ncol - 1, mtry_rsrf_step ) + 1 
      }
      current_varIDs_cart_start <- current_varIDs_cart
      current_varIDs_random_start <- current_varIDs_random
      # print("NEW TRYOUT")
      # print(paste( "RANDOM varIDs", toString(current_varIDs_random) ) )
      # print(paste( "CART varIDs", toString(current_varIDs_cart) )  ) 
      if( saveNodeInformation == TRUE){
        node_information[[nodeID]]$candidate_varIDs_cart_sampled <<- current_varIDs_cart
      }
      # print("__________")
      for( i in loop_start:rsrf_width){
        #Initialize a list of current terminal node IDs
        current_terminal_nodeIDs_tryout <<- NULL
        current_terminal_nodeIDs <<- NULL
        #Make one round of splitting scheme, updates in split_values etc.
        if( i == 0 ){
          #Makes splits only using CART criterion
          randomization_step <<- FALSE
          splitNode(nodeID, current_tree_depth = 0, tryout = TRUE)
          randomization_step <<- TRUE
        }else{
            splitNode(nodeID, current_tree_depth = 0, tryout = TRUE)
        }
        ##Calculate Decrease
        current_decrease <- get_overall_decrease(nodeID, current_terminal_nodeIDs_tryout)
        # print(current_decrease)
        if( current_decrease > best_decrease){
            #UPDATE all best values
            if(saveNodeInformation == TRUE){
              best_candidate <- i
            }
            best_decrease <- current_decrease
            split_values_tryout_best <- split_values
            split_varIDs_tryout_best <- split_varIDs
            split_random_or_not_tryout_best <- split_random_or_not
            child_nodeIDs_tryout_best <- child_nodeIDs
            sampleIDs_best <- sampleIDs
            current_terminal_nodeIDs_best <- current_terminal_nodeIDs
        }
  
        #RESET every global variables that will be updated when growing a tree (for next tryout - round)
        split_values <<- split_values_tryout_start
        split_varIDs <<- split_varIDs_tryout_start
        split_random_or_not <<- split_random_or_not_tryout_start
        child_nodeIDs <<- child_nodeIDs_tryout_start
        sampleIDs <<- sampleIDs_start
        current_varIDs_cart <<- current_varIDs_cart_start
        current_varIDs_random <<- current_varIDs_random_start
      }

      #UPDATE Tree with the best splits tried
      split_values <<- split_values_tryout_best
      split_varIDs <<- split_varIDs_tryout_best
      split_random_or_not <<- split_random_or_not_tryout_best
      child_nodeIDs <<- child_nodeIDs_tryout_best
      current_terminal_nodeIDs <<- current_terminal_nodeIDs_best
      sampleIDs <<- sampleIDs_best
      
      #Only for testing: Print which candidate was best.
      if(saveNodeInformation == TRUE){
        node_information[[nodeID]]$best_candidate <<- best_candidate
        node_information[[nodeID]]$nodeSize <<- length( sampleIDs[[nodeID]])
        if(is.na(split_varIDs[[nodeID]]) == TRUE){
         node_information[[nodeID]]$type <<- "terminal"
        }else{
          node_information[[nodeID]]$type <<- "intermediate"
        }
      }  
    },

    splitNodeInternal = function(nodeID, possible_split_varIDs) {
      ## Empty virtual function
    },

    get_overall_decrease = function(start_nodeID, terminal_nodeIDs){
      #Empty virtual function
    },

    splitNodeInternalRandom = function(nodeID, test_mode) {
      ## Empty virtual function
    },

    estimate = function(nodeID) {
      ## Empty virtual function
    },

    getNodePrediction = function(nodeID) {
      ## Empty virtual function
    },

    predict = function(predict_data) {
      ## Initialize
      num_samples_predict <- predict_data$nrow
      predictions <- list()

      ## For each sample start in root and drop down tree
      for (i in 1:num_samples_predict) {
        nodeID <- 1
        while(TRUE) {
          ## Break if terminal node
          if (nodeID > length(child_nodeIDs) || is.null(child_nodeIDs[[nodeID]])) {
            break
          }

            value <- as.numeric(predict_data$subset(i, split_varIDs[nodeID]))
            if (value <= split_values[nodeID]) {
              nodeID <- child_nodeIDs[[nodeID]][1]
            } else {
              nodeID <- child_nodeIDs[[nodeID]][2]
            }
        }

        ## Add to prediction
        predictions[[i]] <- getNodePrediction(nodeID)
      }
      return(simplify2array(predictions))
    },
    

    predictOOB = function() {
      ## Initialize
      num_samples_predict <- length(oob_sampleIDs)
      predictions <- list()

      ## For each sample start in root and drop down tree
      for (i in 1:num_samples_predict) {
        nodeID <- 1
        while(TRUE) {
          ## Break if terminal node
          if (nodeID > length(child_nodeIDs) || is.null(child_nodeIDs[[nodeID]])) {
            break
          }

            value <- as.numeric(data$subset(oob_sampleIDs[i], split_varIDs[nodeID]))
            if (value <= split_values[nodeID]) {
              nodeID <- child_nodeIDs[[nodeID]][1]
            } else {
              nodeID <- child_nodeIDs[[nodeID]][2]
            }

        }

        ## Add to prediction
        predictions[[i]] <- getNodePrediction(nodeID)
      }
      return(simplify2array(predictions))
    },

    predictionError = function(pred = NULL) {
      ## Empty virtual function
    },

    permuteAndPredictOOB = function(permuted_varID) {
      ## Initialize
      num_samples_predict <- length(oob_sampleIDs)
      predictions <- list()
      permutations <- sample(num_samples_predict)

      ## For each sample start in root and drop down tree
      for (i in 1:num_samples_predict) {
        nodeID <- 1
        while(TRUE) {
          ## Break if terminal node
          if (nodeID > length(child_nodeIDs) || is.null(child_nodeIDs[[nodeID]])) {
            break
          }

      
            if (split_varIDs[nodeID] == permuted_varID) {
              value <- as.numeric(data$subset(oob_sampleIDs[permutations[i]], split_varIDs[nodeID]))
            } else {
              value <- as.numeric(data$subset(oob_sampleIDs[i], split_varIDs[nodeID]))
            }
            if (value <= split_values[nodeID]) {
              nodeID <- child_nodeIDs[[nodeID]][1]
            } else {
              nodeID <- child_nodeIDs[[nodeID]][2]
            }

        }

        ## Add to prediction
        predictions[[i]] <- getNodePrediction(nodeID)
      }
      return(simplify2array(predictions))
    },

    variableImportance = function(type = "permutation") {
      if (type == "permutation") {
        # Prediction error without any permutation
        oob_error <- predictionError()

        # For each variable, prediction error after permutation
        res <- sapply(2:data$ncol, function(varID) {
          pred <- permuteAndPredictOOB(varID)
          oob_error_perm <- predictionError(pred)
          oob_error_perm - oob_error
        })
        names(res) <- data$names[-1]
        res
      } else {
        stop("Only permutation variable importance implemented.")
      }
    },

    #Prints Tree into R-Output
    print_tree_recursive = function(nodeID, rep_character = "  ", current_depth = 1, include_terminal_node_values) {
      if (nodeID > length(child_nodeIDs) || is.null(child_nodeIDs[[nodeID]])) {
          if( include_terminal_node_values == TRUE){
            print(paste(strrep(rep_character,current_depth+2), " This is a terminal node. Value: ", toString( round( split_values[[nodeID]], digits = 3)) ) )
          }
        }else{
        current_nodeIDs <- child_nodeIDs[[nodeID]]
        current_depth <- current_depth + 1

        if(randomization_step == TRUE){
          str_current_random_or_not <- paste("Random? ", toString(split_random_or_not[[nodeID]]))
        }else{
          str_current_random_or_not <- ""
        }
        #Make Prints and recursion
        print(paste(strrep(rep_character,current_depth), toString(nodeID), ")",
                    paste(" x", toString(split_varIDs[nodeID] - 1), sep=""), "<=", toString(round(split_values[nodeID], digits= 7 )),
                    str_current_random_or_not,
                    paste("Child-Node left: ", current_nodeIDs[1], sep="")))
        print_tree_recursive(current_nodeIDs[1], current_depth = current_depth, include_terminal_node_values = include_terminal_node_values)
        print(paste(strrep(rep_character,current_depth), toString(nodeID), ")",
                    paste(" x", toString(split_varIDs[nodeID] - 1), sep=""), " >", toString(round(split_values[nodeID], digits = 7)),
                    str_current_random_or_not,
                    paste("Child-Node right: ", current_nodeIDs[2], sep="")))
        print_tree_recursive(current_nodeIDs[2], current_depth = current_depth, include_terminal_node_values = include_terminal_node_values)
      }
    },

    print_tree = function(rep_character = "  ", include_terminal_node_values = TRUE){
      #Start recursion
      print_tree_recursive(1, rep_character = rep_character, current_depth = 1, include_terminal_node_values = include_terminal_node_values )
      print("Printing completed")
    },
    
    count = function(){
      return( unlist( split_varIDs ) )
    }
    
    )
)
