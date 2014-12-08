#' Creates summary data.frame for feature values across all instances.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{data.frame}].
#' @export
summarizeFeatureValues = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  data = dropNamed(asscenario$feature.values, c("instance_id", "repetition"))
  s = apply(data, 2, getStatistics)
  return(as.data.frame(t(s)))
}
