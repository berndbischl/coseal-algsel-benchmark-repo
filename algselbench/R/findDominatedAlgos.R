#FIXME: think about hiow this should we dwefined if we have stochastic repls
# FIXME: probaly implement what is documented

#' Creates a table that shows the dominance of one algorithm over another one.
#'
#' Stochastic replications are currently aggregated by the mean value.
#' If NAs occur, they are imputed before aggregation either by 10 * cutoff
#' (for runtimes tasks with cutoff) or 10 * <worst performance> for all others.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure for algorithm performance.
#'   Default is first measure in task.
#' @param reduce [\code{logical(1)}]\cr
#'   Should the resulting matrix be reduced to algorithms that a are either dominated by or dominate
#'   another algorithm?
#'   Default is \code{FALSE}.
#' @param type [\code{character(1)}]\cr
#'   Data type of the result object.\cr
#'   \dQuote{logical}: Logical matrix, TRUE means row algorithm dominates column algorithm.\cr
#'   \dQuote{character}: Same information but more human-readable. States how the row relates to the column.
#' @return [\code{matrix}]. See above.
#' @export
findDominatedAlgos = function(astask, measure, reduce = FALSE, type = "logical") {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  perf = astask$algo.perf[[measure]]
  stopifnot(max(perf$repetition) == 1L)
  #FIXME:
  perf = aggregateStochasticAlgoPerf(perf, with.instance.id = FALSE)
  # convert maximization into minimization
  if (astask$desc$maximize[measure])
    perf = -1 * perf
  ns = colnames(perf)
  k = length(ns)
  res = matrix(FALSE, k, k)
  rownames(res) = colnames(res) = ns

  for (i in 1:(k - 1L))
    for (j in (i+1L):k)
      res[i, j] = isDominated(perf[, i], perf[, j])

  # reduce result matrix to entries where something happens
  if (reduce) {
    keep = sapply(1:k, function(i) any(res[i, ]) || any(res[, i]))
    res = res[keep, keep, drop = FALSE]
  }

  if (type == "character") {
    res2 = res
    res2[] = "-"
    res2[res] = "better"
    res2[t(res)] = "worse"
    return(res2)
  } else {
    return(res)
  }

  return(res)
}


# Helper function that checks whether algorithm x is dominated by y.
# here NA = NA, and "real val" is always better than NA.
isDominated = function(x, y) {
  # impute NA perfs of crashes, so we can compare
  x[is.na(x)] = Inf
  y[is.na(y)] = Inf
  # dominated if all values x are at least as bad as y and one x val is really worse
  all(x >= y) && any(x > y)
}
