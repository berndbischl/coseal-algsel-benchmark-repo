#' Creates summary data.frame for algorithm runstatus across all instances.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{data.frame}].
#' @export
summarizeAlgoRunstatus = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  tab = table(asscenario$algo.runs[, c("algorithm", "runstatus")])
  # to percentages
  tab = apply(tab, 1, function(z) 100 * z / sum(z))
  return(as.data.frame(t(tab)))
}
