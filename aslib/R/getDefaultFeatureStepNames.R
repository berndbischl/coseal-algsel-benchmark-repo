#' Returns the default feature step names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getDefaultFeatureStepNames = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  if (is.null(asscenario$desc$default_steps))
    return(names(asscenario$desc$feature_steps))
  else
    return(asscenario$desc$default_steps)
}

