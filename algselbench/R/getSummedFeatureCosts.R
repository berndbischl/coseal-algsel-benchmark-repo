#' Returns feature costs of scenario, summed over all instances.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param feature.steps [\code{character}]\cr
#'   Sum costs only for these selected steps.
#'   Default are all feature steps.
#' @return [\code{character}].
#' @export
getSummedFeatureCosts = function(asscenario, feature.steps) {
  assertClass(asscenario, "ASScenario")
  fs = names(asscenario$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = fs
  else
    assertSubset(feature.steps, fs)
  sum(asscenario$feature.costs[, feature.steps])
}
