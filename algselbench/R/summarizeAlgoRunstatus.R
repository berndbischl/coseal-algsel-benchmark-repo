#' Creates a table that summarizes the runstatus of each algorithm.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{table}]. 
#'  Table, which summarizes the runstatus per algorithm.
#' @export
summarizeAlgoRunstatus = function(astask) {
  checkArg(astask, "ASTask")
  data = astask$algo.runs
  data$runstatus = factor(as.character(data$runstatus), 
    levels = c("ok", "timeout", "memout", "not_applicable", "crash", "other"))
  tab = table(data[, c("algorithm", "runstatus")])
  return(t(apply(tab, 1, function(z) 100 * z / sum(z))))
}
