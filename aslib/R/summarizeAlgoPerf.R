#' Creates summary data.frame for algorithm performance values across all instances.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param measure [\code{character(1)}]\cr
#'   Selected measure.
#'   Default is first measure in scenario.
#' @return [\code{data.frame}].
#' @export
summarizeAlgoPerf = function(asscenario, measure) {
  assertClass(asscenario, "ASScenario")
  measure = checkMeasure(measure, asscenario$desc)
  data = convertAlgoPerfToWideFormat(asscenario$desc, asscenario$algo.runs, measure)
  data = dropNamed(data, c("instance_id", "repetition"))
  s = apply(data, 2, getStatistics)
  return(as.data.frame(t(s)))
}

