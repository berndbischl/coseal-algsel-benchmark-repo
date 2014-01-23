#' Creates a table that summarize the runstatus of the features.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{table}]. 
#'  Table, which summarizes the runstatus per algorithm.
#' @export
summarizeFeatureRunstatus = function(astask) {
  checkArg(astask, "ASTask")
  data = astask$feature.runstatus
  n = ncol(data)
  for (i in 3:(n-1)) {
    data[, i] = factor(as.character(data[, i]), 
      levels = c("ok", "timeout", "memout", "presolved", "crash", "other"))
  }
  tab = t(sapply(data[, 3:(n-1)], table))
  return(t(apply(tab, 1, function(z) 100 * z / sum(z))))
}
