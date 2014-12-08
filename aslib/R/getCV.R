#' Returns number of CV folds.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{integer(1)}].
#' @export
getNumberOfCVFolds = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  return(max(asscenario$cv.splits$fold))
}


#' Returns number of CV repetitions.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{integer(1)}].
#' @export
getNumberOfCVReps = function(asscenario) {
  assertClass(asscenario, "ASScenario")
  return(max(asscenario$cv.splits$repetition))
}




