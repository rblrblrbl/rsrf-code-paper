#' @title Regression Random Split Random Forest Learner
#' @author rblrblrbl
#' @name mlr_learners_regr.rsrf
#'
#' @description
#' Calls [simpleRSRF::simpleRSRF()] from NO CRAN 'simpleRSRF'.
#'
#' @section Initial parameter values:
#' 
#'
#' @section Custom mlr3 defaults:
#' - `rsrf_width`:
#'   - Actual default: NULL
#'   - Adjusted default: 2
#'   - Reason for change: In current implementation, simpleRSRF has no default, but value is required whenever randomization==TRUE 
#'   
#' - `rsrf_depth`:
#'   - Actual default: NULL
#'   - Adjusted default: 2
#'   - Reason for change: Currently, simpleRSRF has no default
#'
#' @section Installation:
#' FIXME: CUSTOM INSTALLATION INSTRUCTIONS. DELETE IF NOT APPLICABLE.
#'
#' @templateVar id regr.rsrf
#' @template learner
#'
#' @references
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerRegrRSRF = R6Class("LearnerRegrRSRF",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        num_trees = p_int(lower = 1L, default = 50L, tags="train"),
        replace = p_lgl(default = TRUE, tags="train"),
        randomization = p_lgl(default = TRUE, tags="train"),
        min_node_size = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags="train"),
        mtry_mode = p_fct( c("nothingfixed", "allfixed", "semifixed"), default = "nothingfixed", tags = "train"),
        mtry_rsrf_step_random = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags="train"),
        mtry_rsrf_step = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags="train"),
        mtry_cart_cart = p_int(lower = 1L, default = NULL, special_vals = list(NULL), tags="train"),
        rsrf_width = p_int(lower = 1L, default = 2L, special_vals = list(NULL), tags="train" ),
        rsrf_depth = p_int(lower = 1L, default = 2L, special_vals = list(NULL), tags="train" ),
        include_CART_CART = p_lgl( default = TRUE, tags = "train"),
        fixed_cart_cart = p_lgl( default = TRUE, tags = "train"),
        splitrule = p_fct( c("Variance"), default = NULL, special_vals = list(NULL), tags="train"),
        num_threads = p_int(1L, default = 1L, tags = c("train", "threads")),
        use_all_data = p_lgl( default = FALSE, tags = c("train")),
        probability = p_lgl( default = FALSE, tags = "train" ),
        test_mode = p_lgl( default = FALSE, tags = "train"),
        saveNodeInformation = p_lgl( default = FALSE, tags = "train" )
      )

      param_set$values = list( rsrf_width = 2L, rsrf_depth = 2L )

      super$initialize(
        id = "regr.rsrf",
        packages = "simpleRSRF",
        feature_types = c("numeric"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("oob_error"),
        man = "mlr3extralearners::mlr_learners_regr.rsrf",
        label = "Random Split Random Forest"
      )
    },
    # FIXME: ADD OOB_ERROR METHOD IF APPLICABLE AND DELETE OTHERWISE
    # SEE mlr3extralearners::LearnerRegrRandomForest FOR AN EXAMPLE
    #' @description
    #' OOB errors are extracted from the model slot FIXME:.
    #' @return `numeric(1)`.
    oob_error = function() {
      pars = self$param_set$get_values(tags = "oob_error")
      # FIXME: Implement oob_error
      return( NA )
    }
  ),
  private = list(
    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      formula = task$formula()
      data = task$data()

      invoke(
        simpleRSRF::simpleRSRF,
        formula = formula,
        data = data,
        .args = pars
      )
    },
    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")

      # get newdata and ensure same ordering in train and predict
      newdata = ordered_features(task, self)

      # Calculate predictions for the selected predict type.
      type = self$predict_type

      # pred = invoke(predict, self$model, newdata = newdata, type = type, .args = pars)
      pred = self$model$predict_wrapper_for_mlr3(newdata, task$target_names)

      # FIXME: ADD PREDICTIONS TO LIST BELOW
      list( response = pred )
    }
  )
)

.extralrns_dict$add("regr.rsrf", LearnerRegrRSRF)
