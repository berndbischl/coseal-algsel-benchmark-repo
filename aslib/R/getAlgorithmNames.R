#' Returns algorithm names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getAlgorithmNames = function(asscenario) {
  c(asscenario$desc$algorithms_deterministic, asscenario$desc$algorithms_stochastic)
}

