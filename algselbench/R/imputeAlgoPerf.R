#' @title Imputes algorithm runs which have NA performance values.
#'
#' @description
#' The following formula is used for imputation:
#' \code{base +- range.scalar * range.span + N(0, sd = jitter * range.span)}\cr
#' With \code{range.span = max - min}.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to impute.
#'   Default is first measure in task.
#' @param base [\code{numeric(1)}]\cr
#'   See formula.
#'   Default is maximum of performance values if measure should be minimized, or minimum for
#'   maximization case.
#' @param range.scalar [\code{numeric(1)}]\cr
#'   See formula.
#'   Default is 0.3.
#' @param jitter [\code{numeric(1)}]\cr
#'   See formula.
#'   Default is 0.
#' @param structure [\code{character(1)}]\cr
#'   What structure should the result have?
#'   Possible values are \dQuote{algo.runs} and \dQuote{algo.perf}.
#'   They correspond to the structures in \code{\link{ASTask}}.
#'   Default is \dQuote{algo.perf}.
#' @return See \code{struture}.
#' @export
imputeAlgoPerf = function(astask, measure, base, range.scalar = 0.3, jitter = 0, structure = "algo.perf") {

  checkArg(astask, "ASTask")
  desc = astask$desc
  ar = astask$algo.runs

  if (missing(measure))
    measure = desc$performance_measures[1]
  else
    checkArg(measure, choices = desc$performance_measures)
  maxi = desc$maximize[measure]
  perf = ar[, measure]

  if (missing(base))
    base = ifelse(maxi, min(perf, na.rm = TRUE), max(perf, na.rm = TRUE))
  else
    checkArg(base, "numeric", len = 1L, na.ok = FALSE)
  checkArg(range.scalar, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(jitter, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(structure, choices = c("algo.runs", "algo.perf"))

  isna = is.na(perf)
  n.na = sum(isna)
  rv = rangeVal(perf, na.rm = TRUE)
  mult = ifelse(maxi, -1, 1)
  newvals = base + mult * range.scalar * rv
  if (jitter) {
    noise = rnorm(n.na, sd = jitter * rv)
    newvals = newvals + noise
  }
  ar[isna, measure] = newvals
  if (structure == "algo.runs")
    return(ar)
  else
    convertAlgoTunsToAlgoPerf(desc, ar, measure)
}
