#' Creates summary data.frame for algorithm performance values across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Selected measure.
#'   Default is first measure in task.
#' @return [\code{data.frame}].
#' @export
summarizeAlgoPerf = function(astask, measure) {
  checkArg(astask, "ASTask")
  measure = checkMeasure(measure, astask$desc)
  data = convertAlgoPerfToWideFormat(astask$desc, astask$algo.runs, measure)
  data = dropNamed(data, c("instance_id", "repetition"))
  s = apply(data, 2, getStatistics)
  return(as.data.frame(t(s)))
}

