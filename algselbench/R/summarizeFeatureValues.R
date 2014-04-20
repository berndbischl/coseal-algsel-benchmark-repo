#' Creates summary data.frame for feature values across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}]. 
#' @export
summarizeFeatureValues = function(astask) {
  checkArg(astask, "ASTask")
  data = dropNamed(astask$feature.values, c("instance_id", "repetition"))
  s = apply(data, 2, getStatistics)
  return(as.data.frame(t(s)))
}
