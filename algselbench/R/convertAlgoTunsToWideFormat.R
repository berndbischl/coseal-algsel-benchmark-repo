#' Converts \code{algo.runs} object of a task to wide format.
#'
#' The first 2 columns are \dQuote{instance_id} and \dQuote{repetition}. The remaining ones are
#' the measured performance values.
#' The feature columns are in the same order as \dQuote{algorithms_deterministic},
#' \dQuote{algorithms_stochastic} in the description object.
#' code{NA} means the performance value is not available, possibly because the algorithm run was aborted.
#' The data.frame is sorted by \dQuote{instance_id}, then \dQuote{repetition}.
#'
#' @param desc [\code{\link{ASTaskDesc}}]\cr
#'   Description object of task.
#' @param algo.runs [\code{data.frame}]\cr
#'   Algo runs data.frame from task.
#' @param measure [\code{character(1)}]\cr
#'   Selected performance measure.
#'   Default is first measure in task.
#' @return [\code{data.frame}].
#' @export
convertAlgoTunsToWideFormat = function(desc, algo.runs, measure) {
  checkArg(desc, "ASTaskDesc")
  checkArg(algo.runs, "data.frame")
  measure = checkMeasure(measure, desc)

  ap = dcast(algo.runs, instance_id + repetition ~ algorithm, value.var = measure)
  # sort rows and cols
  ap = ap[, c("instance_id", "repetition",
    desc$algorithms_deterministic, desc$algorithms_stochastic)]
  sortByCol(ap, c("instance_id", "repetition"))
}
