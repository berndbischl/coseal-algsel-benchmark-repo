#' Returns instance names of task.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{character}].
#' @export
getInstanceNames = function(astask) {
  return(unique(astask$feature.values$instance_id))
}


