#' Convert an ASScenario scenario object to a llama data object with cross-validation folds.
#'
#' For features, mean values are computed across repetitions.
#' For algorithms, repetitions are not supported at the moment and will result in an error.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param measure [\code{character(1)}]\cr
#'   Measure to use for modelling.
#'   Default is first measure in scenario.
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
#'   Default are the splits \code{asscenario$cv.splits}
#' @return Result of calling \code{\link[llama]{input}} with data partitioned into folds.
#' @export
convertToLlamaCVFolds = function(asscenario, measure, feature.steps, add.feature.costs = TRUE, cv.splits) {
  assertClass(asscenario, "ASScenario")
  if (missing(measure))
    measure = asscenario$desc$performance_measures[1]
  else
    assertString(measure)
  if (missing(cv.splits))
    cv.splits = asscenario$cv.splits
  else
    assertClass(cv.splits, "data.frame")
  allsteps = names(asscenario$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = getDefaultFeatureStepNames(asscenario)
  else
    assertSubset(feature.steps, allsteps)

  reps = max(cv.splits$repetition)
  if (reps > 1L)
    stopf("llama can currently not handle CVs with repetitions, but you used reps = %i!", reps)

  folds = cv.splits

  llamaFrame = convertToLlama(asscenario, measure = measure,
    feature.steps = feature.steps, add.feature.costs = add.feature.costs)

  nfolds = length(unique(folds$fold))
  rownames(folds) = folds$instance_id
  splitFactors = folds[llamaFrame$data$instance_id, "fold"]
  parts = split(llamaFrame$data, splitFactors)

  minimize = !asscenario$desc$maximize[measure]

  return(c(llamaFrame,
            list(train = lapply(1:nfolds, function(x) {
                    part = unsplit(parts[-x], folds$fold[folds$fold!=x])
                    perfs = subset(part, select=llamaFrame$performance)
                    order = names(sort(sapply(perfs, sum), decreasing=!minimize))
                    optfun = if(minimize) { which.min } else { which.max }
                    part$best = factor(apply(perfs[order], 1, function(x) { order[optfun(unlist(x))] }))
                    return(part)
                }),
             test = lapply(1:nfolds, function(x) {
                    part = parts[[x]]
                    otherPart = unsplit(parts[-x], folds$fold[folds$fold!=x])
                    perfs = subset(part, select=llamaFrame$performance)
                    order = names(sort(sapply(subset(otherPart, select=llamaFrame$performance), sum), decreasing=!minimize))
                    optfun = if(minimize) { which.min } else { which.max }
                    part$best = factor(apply(perfs[order], 1, function(x) { order[optfun(unlist(x))] }))
                    return(part)
                 }))))
}
