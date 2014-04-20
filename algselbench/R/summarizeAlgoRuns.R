#' Creates summary data.frame for algorithm performance values across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Selected measure.
#'   Default is first measure in task.
#' @return [\code{data.frame}].
#' @export
summarizeAlgoRuns = function(astask, measure) {
  checkArg(astask, "ASTask")
  measure = checkMeasure(measure, astask$desc)
  ap = convertAlgoTunsToWideFormat(astask$desc, astask$algo.runs, measure)
  ap = dropNamed(ap, c("repetition", "instance_id"))

  funs = list(
    obs = function(x) length(x),
    nas = function(x) sum(is.na(x)),
    run_ok = function(x) 100 * mean(!is.na(x)),
    min = function(x) min(x, na.rm = TRUE),
    qu_1st = function(x) quantile(x, 0.25, na.rm = TRUE),
    med = function(x) median(x, na.rm = TRUE),
    mean = function(x) mean(x, na.rm = TRUE),
    qu_3rd  = function(x) quantile(x, 0.75, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE),
    coeff_var = function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  )
  # put vector in, get vector of stats out
  getStatistics = function(x) sapply(funs, function(f) f(x))
  s = apply(ap, 2, getStatistics)
  setColNames(as.data.frame(t(s)), names(funs))
}

