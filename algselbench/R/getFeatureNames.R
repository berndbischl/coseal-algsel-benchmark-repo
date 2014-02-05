#' Returns feature names of task.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{character}].
#' @export
getFeatureNames = function(astask) {
  c(astask$desc$features_deterministic, astask$desc$features_stochastic)
}
