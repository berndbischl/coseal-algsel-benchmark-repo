#' Returns algorithm names of task.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{character}].
#' @export
getAlgorithmNames = function(astask) {
  c(astask$desc$algorithms_deterministic, astask$desc$algorithms_stochastic)
}

