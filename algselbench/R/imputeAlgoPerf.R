#' @title Imputes algorithm runs which have NA performance values.
#'
#' @description
#' The following formula is used for imputation
#' \code{range.span = max - min}
#' \code{base +- range.scalar * range.span + N(0, sd = jitter * range.span)}.
#' jitter is a  
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to impute.
#'   Default is first measure in task.
#' @param log [\code{(1)}]\cr
#'   Should the performance values be log10-transformed in the plot?
#'   Default is FALSE.
#' @param na.impute [\code{logical(1)}]\cr
#'   Should the values of algorithm runs with non-ok runstatus (missing performance 
#'   values) be imputed? If yes, imputation is done via max + scalar * (max - min) for
#'   minimization problems and via min - scalar * (max - min) for maximization problems.
#'   Default is TRUE.
#' @return ggplot2 plot object.
#' @export
#' @param perf [\code{matrix}]\cr
#' @param perf [\code{matrix}]\cr
#'   Algorithm performance matrix, see \code{algo.perf} in \code{\link{ASTask}}.
#' @return [\code{matrix}]. Same format as \code{perf}.
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
