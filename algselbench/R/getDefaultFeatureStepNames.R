#' Returns the default feature step names of task.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{character}].
#' @export
getDefaultFeatureStepNames = function(astask) {
  checkArg(astask, "ASTask")
  if (is.null(astask$desc$default_feature_steps))
    return(names(astask$desc$feature_steps))
  else
    return(astask$desc$default_feature_steps)
}

