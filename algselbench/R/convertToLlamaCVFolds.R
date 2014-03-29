#' Convert an ASTask task object to a llama data object with cross-validation folds.
#'
#' For stochastic algorithms and features, mean values are computed across repetitions.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to use for modelling.
#'   Default is first measure in task.
#' @param feature.steps [\code{character}]\cr
#'   Which feature steps are allowed?
#'   Default are the default feature steps or all steps
#'   in case no defaults were defined.
#' @param add.feature.costs [\code{logical(1)}]\cr
#'   See \code{\link{convertToLlama}}.
#'   Default is \code{TRUE}.
#' @param cv.splits [\code{data.frame}]\cr
#'   Data frame defining the split of the data into cross-validation folds,
#'   as returned by \code{\link{createCVSplits}}.
#'   Default are the splits \code{astask$cv.splits}
#' @return Result of calling \code{\link[llama]{input}} with data partitioned into folds.
#' @export
convertToLlamaCVFolds = function(astask, measure, feature.steps, add.feature.costs = TRUE, cv.splits) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  if (missing(cv.splits))
    cv.splits = astask$cv.splits
  else
    checkArg(cv.splits, "data.frame")
  allsteps = names(astask$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = getDefaultFeatureStepNames(astask)
  else
    checkArg(feature.steps, subset = allsteps)

  reps = max(cv.splits$rep)
  if (reps > 1L)
    stopf("llama can currently not handle CVs with repetitions, but you used reps = %i!", reps)

  folds = cv.splits

  llamaFrame = convertToLlama(astask, measure = measure,
    feature.steps = feature.steps, add.feature.costs = add.feature.costs)

  nfolds = length(unique(folds$fold))
  rownames(folds) = folds$instance_id
  splitFactors = folds[llamaFrame$data$instance_id, "fold"]
  parts = split(llamaFrame$data, splitFactors)

  return(c(llamaFrame,
            list(train = lapply(1:nfolds, function(x) { return(unsplit(parts[-x], folds$fold[folds$fold!=x])) }),
                 test = lapply(1:nfolds, function(x) { return(parts[[x]]) }))))
}
