#' Returns instance names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{character}].
#' @export
getInstanceNames = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  return(unique(asscenario$feature.values$instance_id))
}


