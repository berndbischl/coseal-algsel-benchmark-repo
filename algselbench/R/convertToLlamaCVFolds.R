#' Convert an ASTask task object to a llama data object with cross-validation folds.
#'
#' For stochastic algorithms and features, mean values are computed across repetitions.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param folds [\code{data.frame(1)}]\cr
#'   Data frame defining the split of the data into cross-validation folds, as returned by \code{\link{createCVSplits}}.
#' @param measure [\code{character(1)}]\cr
#'   Measure to use for modelling.
#'   Default is first measure in task.
#' @return Result of calling \code{\link[llama]{input}} with data partitioned into folds.
#' @export
convertToLlamaCVFolds = function(astask, folds, measure) {
  checkArg(folds, "data.frame")

  llamaFrame = convertToLlama(astask, measure)

  nfolds = length(unique(folds$fold))
  parts = split(llamaFrame$data, folds$fold)

  return(c(llamaFrame,
            list(train = lapply(1:nfolds, function(x) { return(unsplit(parts[-x], folds$fold[folds$fold!=x])) }),
                 test = lapply(1:nfolds, function(x) { return(parts[[x]]) }))))
}
