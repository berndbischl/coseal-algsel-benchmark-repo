#' @title Imputes algorithm performance for runs which have NA performance values.
#'
#' @description
#' The following formula is used for imputation:
#' \code{base +- range.scalar * range.span + N(0, sd = jitter * range.span)}\cr
#' With \code{range.span = max - min}.
#'
#' Returns an object like \code{algo.runs} of \code{asscenario}, but drops
#' the runstatus and all other measures.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param measure [\code{character(1)}]\cr
#'   Measure to impute.
#'   Default is first measure in scenario.
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
#' @param impute.zero.vals [\code{logical(1)}]\cr
#'   Should values which are exactly 0 be imputed to 1e-6?
#'   This allows to take the logarithm later on, handy for subsequent visualizations.
#'   Note that this really only makes sense for non-negative measures!
#'   Default is FALSE.
#' @return [\code{data.frame}].
#' @export
imputeAlgoPerf = function(asscenario, measure, base = NULL, range.scalar = 0.3, jitter = 0,
  impute.zero.vals = FALSE) {

  assertClass(asscenario, "ASScenario")
  desc = asscenario$desc
  measure = checkMeasure(measure, asscenario$desc)
  ar = asscenario$algo.runs
  maxi = desc$maximize[measure]
  # reduce to relevant cols
  ar = ar[, c("instance_id", "repetition", "algorithm", measure)]
  perf = ar[, measure]

  # base is either min or max perf
  if (is.null(base))
    base = ifelse(maxi, min(perf, na.rm = TRUE), max(perf, na.rm = TRUE))
  else
    assertNumber(base)
  assertNumber(range.scalar, lower = 0)
  assertNumber(jitter, lower = 0)
  assertFlag(impute.zero.vals)

  isna = is.na(perf)
  n.na = sum(isna)
  rv = rangeVal(perf, na.rm = TRUE)
  mult = ifelse(maxi, -1, 1)
  newvals = base + mult * range.scalar * rv

  # maybe add jitter
  if (jitter != 0) {
    noise = rnorm(n.na, sd = jitter * rv)
    newvals = newvals + noise
  }
  ar[isna, measure] = newvals

  ar = imputeZeroVals(ar, measure, impute.zero.vals)
  return(ar)
}
