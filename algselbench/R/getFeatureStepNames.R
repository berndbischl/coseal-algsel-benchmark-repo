#' Returns feature step names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getFeatureStepNames = function(asscenario) {
  checkArg(asscenario, "ASScenario")
  return(names(asscenario$desc$feature_steps))
}

