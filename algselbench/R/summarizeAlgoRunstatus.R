#' Creates summary data.frame for algorithm runstatus across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}]. 
#' @export
summarizeAlgoRunstatus = function(astask) {
  checkArg(astask, "ASTask")
  tab = table(astask$algo.runs[, c("algorithm", "runstatus")])
  # to percentages
  tab = apply(tab, 1, function(z) 100 * z / sum(z))
  return(as.data.frame(t(tab)))
}
