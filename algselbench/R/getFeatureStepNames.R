#' Returns feature step names of task.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{character}].
#' @export
getFeatureStepNames = function(astask) {
  checkArg(astask, "ASTask")
  return(names(astask$desc$feature_steps))
}

