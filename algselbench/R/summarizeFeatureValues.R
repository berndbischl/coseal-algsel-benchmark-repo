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
  values = astask$feature.values[,-c(1:2)]
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
  colnames(result) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
    "Max.", "Std. Dev.", "CoV.", "NA's", "Obs.")
  return(as.data.frame(result))
}
