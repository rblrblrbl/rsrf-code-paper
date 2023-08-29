#Input: mlr3 learner, a mlr3 task (with rows "use" for training and "holdout" for evaluation)
#Return: prediction score on holdout set (mse for regression)
mlr3simulateFromLearner <- function( task, learner ){
  learner$train( task ) #train learner on rows "use"
  prediction <- learner$predict( task, task$row_roles$holdout ) #use data with row_roles "holdout" for prediction
  return( prediction$score() )
}


#Input: mlr3 task (with rows "use" for training and "holdout" for evaluation), mlr3 learner (with some parameters set via to_tune() )
#Return: a data.table object returned vom instance$archive sortiert nach tuning parametern und IDs 1,2,.. entsprechend dieser Sortierung (für spätere Auswertung)
#parallelize therein using future
findOpt <- function( task , learner, design = NULL, resolution = NULL, batch_size = 5){
  use_ids <- task$row_roles$use
  holdout_ids <- task$row_roles$holdout
  train_set <- list(use_ids)
  holdout_set  <- list(holdout_ids)
  #Attention: This is a workaround, as I do not know how to use "holdout" samples within resampling class.
  #All holdout rows are set to "use"
  task$set_row_roles( holdout_ids, roles = c("use") )
  custom = rsmp("custom")
  custom$instantiate(task, train_set, holdout_set)
  if( is.null(design) ){
    instance = tune(
      method = tnr("grid_search", resolution = resolution, batch_size = batch_size),
      task = task,
      learner = learner,
      resampling = custom,
      measures = msr("regr.mse")
    )
  }else{
    instance = tune(
      method = tnr("design_points", design = design, batch_size = batch_size),
      task = task,
      learner = learner,
      resampling = custom,
      measures = msr("regr.mse")
    )
  }
  results <- data.table::as.data.table( instance$archive )
  #Sort data.table to make in comparamble!
  params_in_results <- colnames(results)[ colnames(results) %in% learner$param_set$ids() ]
  data.table::setorderv( results, cols = params_in_results )
  results[,ID := .I]
  return( results )
}