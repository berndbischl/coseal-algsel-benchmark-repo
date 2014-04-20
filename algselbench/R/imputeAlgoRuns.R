#' @title Imputes algorithm runs which have NA performance values.
#'
#' @description
#' The following formula is used for imputation:
#' \code{base +- range.scalar * range.span + N(0, sd = jitter * range.span)}\cr
#' With \code{range.span = max - min}.
#'
#' Returns an object like \code{algo.runs} of \code{astask}, but drops
#' the runstatus and all other measures.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to impute.
#'   Default is first measure in task.
#' @param base [\code{numeric(1)}]\cr
#'   See formula.
#'   Default is \code{NULL}, which means maximum of performance values if measure should be minimized,
#'   or minimum for maximization case.
#' @param range.scalar [\code{numeric(1)}]\cr
#'   See formula.
#'   Default is 0.3.
#' @param jitter [\code{numeric(1)}]\cr
#'   See formula.
#'   Default is 0.
#' @return [\code{data.frame}].
#' @export
imputeAlgoRuns = function(astask, measure, base = NULL, range.scalar = 0.3, jitter = 0) {

  checkArg(astask, "ASTask")
  desc = astask$desc
  measure = checkMeasure(measure, astask$desc)
  ar = astask$algo.runs
  maxi = desc$maximize[measure]
  # reduce to relevant cols
  ar = ar[, c("instance_id", "repetition", "algorithm", measure)]
  perf = ar[, measure]

  # base is either min or max perf
  if (is.null(base))
    base = ifelse(maxi, min(perf, na.rm = TRUE), max(perf, na.rm = TRUE))
  else
    checkArg(base, "numeric", len = 1L, na.ok = FALSE)
  checkArg(range.scalar, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(jitter, "numeric", len = 1L, na.ok = FALSE, lower = 0)

  isna = is.na(perf)
  n.na = sum(isna)
  rv = rangeVal(perf, na.rm = TRUE)
  mult = ifelse(maxi, -1, 1)
  newvals = base + mult * range.scalar * rv

  # maybe add jitter
  if (jitter) {
    noise = rnorm(n.na, sd = jitter * rv)
    newvals = newvals + noise
  }
  ar[isna, measure] = newvals

  return(ar)
}
