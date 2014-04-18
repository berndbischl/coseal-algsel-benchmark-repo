#' Creates a table that gives an overview of the performance values 
#' per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure that's been used for analyzing the algorithm performances.
#'   Default is first measure in task.
#' @return [\code{data.frame}]. 
#'  Data frame, which gives an overview of the performance values.
#' @export
summarizeAlgoRuns = function(astask, measure) {
  checkArg(astask, "ASTask")
  data = astask$algo.runs
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  return(summarizeAlgos(data, measure))
}


## Helper function that actually creates the overview.
summarizeAlgos = function(data, measure) {
  splitted.data = split(data, data$algorithm)
  splitted.data = c(splitted.data, allAlgorithms = list(data))
  varCoeff = function(x) sd(x) / mean(x)
  solved = function(x) 100 * mean(as.character(x) == "ok")
  foo = function(x, aggr) {
    aggr(na.omit(x[[measure]]))
  }
  result = sapply(splitted.data, nrow)
  result = cbind(result, sapply(splitted.data, 
    function(z) sum( is.na(z[[measure]]) ))) 
  result = cbind(result, sapply(splitted.data, 
    function(z) solved(z$runstatus)))
  result = cbind(result, sapply(splitted.data, function(z) foo(z, min)))
  result = cbind(result, sapply(splitted.data, 
    function(z) foo(z, function(a) quantile(a, 0.25))))
  result = cbind(result, sapply(splitted.data, function(z) foo(z, median)))
  result = cbind(result, sapply(splitted.data, function(z) foo(z, mean)))
  result = cbind(result, sapply(splitted.data, 
    function(z) foo(z, function(a) quantile(a, 0.75))))
  result = cbind(result, sapply(splitted.data, function(z) foo(z, max)))
  result = cbind(result, sapply(splitted.data, function(z) foo(z, sd))) 
  result = cbind(result, sapply(splitted.data, function(z) foo(z, varCoeff))) 
  colnames(result) = c("obs", "NAs", "run_ok", "min", "1st_qu", "median", 
    "mean", "3rd_qu", "max", "std_dev", "co_var")
  return(as.data.frame(result))
}
