#' Returns feature costs of task, summed over all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param feature.steps [\code{character}]\cr
#'   Sum costs only for these selected steps.
#'   Default are all feature steps.
#' @return [\code{character}].
#' @export
getSummedFeatureCosts = function(astask, feature.steps) {
  checkArg(astask, "ASTask")
  fs = names(astask$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = fs
  else
    checkArg(feature.steps, subset=fs)
  sum(astask$feature.costs[, feature.steps])
}
