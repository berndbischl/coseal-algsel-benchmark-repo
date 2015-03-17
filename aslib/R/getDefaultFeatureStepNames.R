#' Returns the default feature step names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getDefaultFeatureStepNames = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  assertVector(asscenario$desc$default_steps)
  return(asscenario$desc$default_steps)
}

