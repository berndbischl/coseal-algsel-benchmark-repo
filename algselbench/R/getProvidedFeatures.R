#' Return features that are useable for a given set of feature steps.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param steps [\code{character}]\cr
#'   Feature steps.
#'   Default are all feature steps.
#' @return [\code{character}].
#' @export
getProvidedFeatures = function(asscenario, steps) {
  checkArg(asscenario, "ASScenario")
  if (missing(steps))
    steps = names(asscenario$desc$feature_steps)
  else
    checkArg(steps, subset = names(asscenario$desc$feature_steps))
  allfeats = getFeatureNames(asscenario)
  step.list = asscenario$desc$feature_steps
  allsteps = names(step.list)
  notsteps = setdiff(allsteps, steps)
  notfeatures = Reduce(union, step.list[notsteps])
  return(setdiff(allfeats, notfeatures))
}


