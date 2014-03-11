#' Convert an ASTask task object to a llama data object with cross-validation folds.
#'
#' For stochastic algorithms and features, mean values are computed across repetitions.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to use for modelling.
#'   Default is first measure in task.
#' @param cv.splits [\code{data.frame}]\cr
#'   Data frame defining the split of the data into cross-validation folds,
#'   as returned by \code{\link{createCVSplits}}.
#'   Default are the splits \code{astask$cv.splits}
#' @return Result of calling \code{\link[llama]{input}} with data partitioned into folds.
#' @export
convertToLlamaCVFolds = function(astask, measure, cv.splits) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  if (missing(cv.splits))
    cv.splits = astask$cv.splits
  else
    checkArg(cv.splits, "data.frame")

  folds = cv.splits

  llamaFrame = convertToLlama(astask, measure)

  nfolds = length(unique(folds$fold))
  parts = split(llamaFrame$data, folds$fold)

  return(c(llamaFrame,
            list(train = lapply(1:nfolds, function(x) { return(unsplit(parts[-x], folds$fold[folds$fold!=x])) }),
                 test = lapply(1:nfolds, function(x) { return(parts[[x]]) }))))
}
