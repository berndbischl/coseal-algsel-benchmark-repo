#' Returns feature names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getFeatureNames = function(asscenario) {
  checkArg(asscenario, "ASScenario")
  c(asscenario$desc$features_deterministic, asscenario$desc$features_stochastic)
}
