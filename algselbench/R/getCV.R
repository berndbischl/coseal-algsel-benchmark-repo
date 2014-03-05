#' Returns number of CV folds.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{integer(1)}].
#' @export
getNumberOfCVFolds = function(astask) {
  checkArg(astask, "ASTask")
  return(max(astask$cv.splits$fold))
}


#' Returns number of CV repetitions.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{integer(1)}].
#' @export
getNumberOfCVReps = function(astask) {
  checkArg(astask, "ASTask")
  return(max(astask$cv.splits$repetition))
}




