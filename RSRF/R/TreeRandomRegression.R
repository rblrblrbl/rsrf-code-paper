
##' @title Regression tree class
##' @description Subclass for regression tree.
##' Contains all fields and methods used special for regression trees.
TreeRandomRegression <- setRefClass("TreeRandomRegression",
  contains = "Tree",
  fields = list(),
  methods = list(

    splitNodeInternal = function(nodeID, possible_split_varIDs) {
      #print( paste( "(CART) Splitting nodeID", nodeID, "at possible_splits", toString( possible_split_varIDs ) ) )
      ## Check node size, stop if maximum reached
      if (length(sampleIDs[[nodeID]]) <= min_node_size) {
        return(NULL)
      }

      ## Stop if node is pure
      unique_response <- unique(data$subset(sampleIDs[[nodeID]], 1))
      if (length(unique_response) == 1) {
        return(NULL)
      }

      ## Find best split, stop if no decrease of impurity
      return(findBestSplit(nodeID, possible_split_varIDs))
    },

    splitNodeInternalRandom = function(nodeID, test_mode = FALSE, possible_split_varIDs = NULL){
      #print( paste( "(Random) Splitting nodeID", nodeID, "at possible_splits", toString( possible_split_varIDs ) ) )
     ## Check node size, stop if minimum reached
      if (length(sampleIDs[[nodeID]]) <= min_node_size) {
        return(NULL)
      }
      
      # For testing, not place any split, thus omits placing a random split.
      if(test_mode == TRUE){
        result <- NULL
        return(result)
      }
      
      # Stop if node is pure
      unique_response <- unique(data$subset(sampleIDs[[nodeID]], 1))
      if (length(unique_response) == 1) {
        return(NULL)
      }
      result <- NULL
      if( is.null(possible_split_varIDs) ){
        result$varID <- as.integer( sample(data$ncol-1, 1, replace = FALSE) + 1 )
      }else
      {
        result$varID <- as.integer( possible_split_varIDs[ sample.int( length(possible_split_varIDs ), size = 1) ] )
      }
      data_subset <- data$subset(sampleIDs[[nodeID]], result$varID)
      #If data_subset contains only unique values: do not split
      if( length(unique(data_subset)) == 1 ){
        return(NULL)
      }
      #Remove largest entries in data_subset: guarantees that we do not create an empty node
      data_subset_remove_minmax <- data_subset[ which( data_subset < max(data_subset) )]
      result$value <- data_subset_remove_minmax[ sample.int( length( data_subset_remove_minmax ) , size = 1 ) ]
      #print( paste0( "Random split at coordinate ", result$varID, " and split position : ", result$value ) )
      return(result)
    },

    findBestSplit = function(nodeID, possible_split_varIDs) {
      ## Initialize
      best_split <- NULL
      best_split$decrease <- -1
      best_split$varID <- -1
      best_split$value <- -1

      ## Get response
      response <- data$subset(sampleIDs[[nodeID]], 1)

      ## For all possible variables
      for (i in 1:length(possible_split_varIDs)) {
        split_varID <- possible_split_varIDs[i]
        data_values <- data$subset(sampleIDs[[nodeID]], split_varID)
        best_split = findBestSplitValueOrdered(split_varID, data_values, best_split, response)
      }

      if (best_split$varID < 0) {
        ## Stop if no good split found
        return(NULL)
      } else {
        ## Return best split
        result <- NULL
        result$varID <- as.integer(best_split$varID)
        result$value <- best_split$value
        return(result)
      }
    },

    findBestSplitValueOrdered = function(split_varID, data_values, best_split, response) {
      ## For all possible splits
      possible_split_values <- unique(data_values)
      for (j in 1:length(possible_split_values)) {
        split_value <- possible_split_values[j]

        ## Sum responses in childs
        idx <- data_values <= split_value
        response_left <- response[idx]
        response_right <- response[!idx]

        ## Skip if one child empty
        if (length(response_left) == 0 | length(response_right) == 0) {
          next
        }

        if (splitrule == "Variance") {
          ## Decrease of impurity
          decrease <- sum(response_left)^2/length(response_left) +
            sum(response_right)^2/length(response_right)
        } else {
          stop("Unknown splitrule.")
        }

        ## Use this split if better than before
        if (decrease > best_split$decrease) {
          best_split$value <- split_value
          best_split$varID <- split_varID
          best_split$decrease <- decrease
        }
      }
      return(best_split)
    },

    estimate = function(nodeID) {
      ## Return mean response
      return(mean(data$subset(sampleIDs[[nodeID]], 1)))
    },

    getNodePrediction = function(nodeID) {
      return(split_values[nodeID])
    },

    get_overall_decrease = function(nodeID, terminal_nodeIDs){
      #Input: NodeID and all (current) terminal node IDs corresponding to that nodeID
      #Returns: (Empirical) impurity decrease
      response <- data$subset( sampleIDs[[nodeID]], 1)
      response_terminals <- lapply( terminal_nodeIDs, function(i){ return( data$subset(sampleIDs[[i]], 1) ) })
      mean_start_node <- mean( response )
      decrease <- sum( sapply( response_terminals, function(x){ return(length(x) * ( mean( x ) - mean_start_node )^2 ) } ) ) / length(response)
      return( decrease )
    },

    predictionError = function(pred = NULL) {
      if( use_all_data == TRUE){
        return("OOB Prediction is not available because all data has been used")
      }
      if (is.null(pred)) {
        pred <- predictOOB()
      }
      sum((pred - data$subset(oob_sampleIDs, 1))^2, na.rm = TRUE) / length(oob_sampleIDs)
    })

)
