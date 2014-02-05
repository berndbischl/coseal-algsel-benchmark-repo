#' Calculate aggregated performances per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param aggr [\code{function}]\cr
#'   Agregation function.
#'   Default is \code{mean}
#' @return [\code{data.frame}]. Rows correspond to algorithms and are named in this fashion.
#'   For each performance measure two columns are created: One for the aggregated performance values,
#'   one for the percentage of missing values.
#' @export
calcAggrPerfValues = function(astask, aggr) {
  checkArg(astask, "ASTask")
  if (missing(aggr))
    aggr = function(x) median(x, na.rm = TRUE)
  else
    checkArg(aggr, "function")

  ars = astask$algo.runs
  measures = astask$desc$performance_measures
  res = ddply(ars, c("algorithm"), function(d) {
    row = list()
    row = lapply(measures, function(m) {
      p = d[, m]
      setNames(
        c(aggr(p), nas = mean(is.na(p) * 100)),
        c(m, sprintf("%s.nas", m))
      )
    })
    as.data.frame(as.list(unlist(row)))
  })
  rownames(res) = res$algorithm
  res$algorithm = NULL
  return(res)
}
