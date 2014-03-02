#' Creates a table that summarize the runstatus of the features.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{table}]. 
#'  Table, which summarizes the runstatus per algorithm.
#' @export
summarizeFeatureRunStatus = function(astask) {
  checkArg(astask, "ASTask")
  data = astask$feature.runstatus
  n = ncol(data)
  for (i in 3:n) {
    data[, i] = factor(as.character(data[, i]), 
      levels = c("ok", "timeout", "memout", "presolved", "crash", "other"))
  }
  if (n == 3) {
    tab = rbind(table(data[, 3]))
    rownames(tab) = colnames(data)[3]
    return(100 * tab / sum(tab))
  } else {
    tab = t(sapply(data[, 3:n], table))
    return(t(apply(tab, 1, function(z) 100 * z / sum(z))))
  }
}
