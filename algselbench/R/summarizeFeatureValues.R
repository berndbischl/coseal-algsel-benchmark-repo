#' Creates a table that gives an overview of the feature values
#' across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}]. 
#'  Overview of the feature values.
#' @export
summarizeFeatureValues = function(astask) {
  checkArg(astask, "ASTask")
  featValues = astask$feature.values
  featStatus = astask$feature.runstatus
  return(summarizeFeatures(featValues[,-c(1:2)], 
    featStatus[,-c(1:2, ncol(featStatus))]))
}

## Helper function that does the summary of the feature
## values, depending on the feature values and their
## runstatus.
summarizeFeatures = function(values, status) {
  varCoeff = function(x) sd(x) / mean(x)
  foo = function(x, aggr)
    aggr(na.omit(x))
  result = sapply(values, function(z) foo(z, min))
  result = cbind(result, sapply(values, 
    function(z) foo(z, function(a) quantile(a, 0.25))))
  result = cbind(result, sapply(values, function(z) foo(z, median)))
  result = cbind(result, sapply(values, function(z) foo(z, mean)))
  result = cbind(result, sapply(values,
    function(z) foo(z, function(a) quantile(a, 0.75))))
  result = cbind(result, sapply(values, function(z) foo(z, max)))
  result = cbind(result, sapply(values, function(z) foo(z, sd))) 
  result = cbind(result, sapply(values, function(z) foo(z, varCoeff))) 
  result = cbind(result, sapply(values, function(z) sum(is.na(z)))) 
  result = cbind(result, sapply(values, length))
  result = cbind(result, sapply(status,
    function(z) 100 * mean(as.character(z) == "ok")))
  colnames(result) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
    "Max.", "Std. Dev.", "CoV.", "NA's", "Obs.", "Run OK (%)")
  return(as.data.frame(result))
}

