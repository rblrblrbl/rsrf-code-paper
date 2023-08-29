#' @title Regression Interaction Forests Learner
#' @author RomanHornung
#' @name mlr_learners_regr.intf
#'
#' @description
#' Interaction Forests for Regression. Ignoring EIM (Variable importance) within interaction forest.
#' Calls [diversityForest::interactionfor()] from: CRAN \CRANpkg{diversityForest}.
#'
#' @section Initial parameter values:
#'
#' @section Custom mlr3 defaults:
#' - `num.trees`:
#'   - Actual default: 2000 (without EIM)
#'   - Adjusted default: 500 (as in ranger)
#'   - Reason for change: 
#' - `importance`:
#'   - Actual default: both 
#'   - Adjusted default: none
#'   - Reason for change: No EIM.
#' - `simplify.large.n`:
#'   - Actual default: both 
#'   - Adjusted default: none
#'   - Reason for change: No EIM.
#' - `num.threads`:
#'   - Actual default:  NULL, triggering auto-detection of the number of CPUs.
#'   - Adjusted default: 1
#'   - Reason for change: Conflicting with parallelization via future.
#'
#' @section Installation:
#'
#' @templateVar id regr.intf
#' @template learner
#'
#' @references
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerRegrinteractionforest = R6Class("LearnerRegrinteractionforest",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
          always.split.variables       = p_uty(tags = "train"),
          holdout                      = p_lgl(default = FALSE, tags = "train"),
          importance                   = p_fct(c("none"), default = "none", tags = "train"),
          keep.inbag                   = p_lgl(default = FALSE, tags = "train"),
          max.depth                    = p_int(default = NULL, lower = 0L, special_vals = list(NULL), tags = "train"),
          min.node.size                = p_int(1L, default = 5L, special_vals = list(NULL), tags = "train"),
          npairs                       = p_int(1L, default = NULL, special_vals = list(NULL), tags = "train"),
          num.threads                  = p_int(1L, default = 1L, tags = c("train", "predict", "threads")),
          num.trees                    = p_int(1L, default = 500L, tags = c("train")),
          oob.error                    = p_lgl(default = TRUE, tags = "train"),
          quantreg                     = p_lgl(default = FALSE, tags = "train"),
          replace                      = p_lgl(default = TRUE, tags = "train"),
          sample.fraction              = p_dbl(0L, 1L, tags = "train"),
          simplify.large.n             = p_lgl(default = FALSE, tags = "train"),
          seed                         = p_int(default = NULL, special_vals = list(NULL), tags = c("train", "predict")),
          splitrule                    = p_fct(c("variance", "extratrees", "maxstat"), default = "variance", tags = "train"),
          verbose                      = p_lgl(default = TRUE, tags = c("train", "predict")),
          write.forest                 = p_lgl(default = TRUE, tags = "train")
      )
      param_set$values = list(
        num.trees = 500L,
        importance = "none",
        simplify.large.n = FALSE,
        num.threads = 1L
        )

      super$initialize(
        id = "regr.intf",
        packages = "diversityForest",
        feature_types = c("numeric"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c(),
        man = "mlr3extralearners::mlr_learners_regr.intf",
        label = "Interaction Forest"
      )
    }
  ),
  private = list(
    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      formula = task$formula()
      data = task$data()

      # TRAIN CALL. CHECK OTHER LEARNERS FOR WHAT CAN BE DONE HERE
      # USE THE mlr3misc::invoke FUNCTION (IT'S SIMILAR TO do.call())

      invoke(
        diversityForest::interactionfor,
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

      prediction = invoke(predict, self$model, data = newdata, type = self$predict_type, .args = pars)
      list(response = prediction$predictions)
    }
  )
)

.extralrns_dict$add("regr.intf", LearnerRegrinteractionforest)
