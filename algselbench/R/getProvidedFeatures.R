#' Return features that are useable for a given set of feature steps.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param steps [\code{character}]\cr
#'   Feature steps.
#'   Default are all feature steps.
#' @return [\code{character}].
#' @export
getProvidedFeatures = function(astask, steps) {
  checkArg(astask, "ASTask")
  if (missing(steps))
    steps = names(astask$desc$feature_steps)
  else
    checkArg(steps, subset = names(astask$desc$feature_steps))
  allfeats = getFeatureNames(astask)
  step.list = astask$desc$feature_steps
  allsteps = names(step.list)
  notsteps = setdiff(allsteps, steps)
  notfeatures = Reduce(union, step.list[notsteps])
  return(setdiff(allfeats, notfeatures))
}


