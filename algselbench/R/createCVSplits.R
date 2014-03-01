#' @title Create cross-validation splits for a task.
#'
#' @describtion
#' Create a data.frame that defines cross-validation splits for a task,
#'
#' and potentially store it in an ARFF file.
#'
#' The \code{mlr} package is used to generate the splits, see
#' \code{\link{makeResampleDesc}} and \code{\link{makeResampleInstance}}.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param reps [\code{integer}]\cr
#'   CV repetitions.
#'   Default is 1.
#' @param folds [\code{integer}]\cr
#'   CV folds.
#'   Default is 10.
#' @param file [\code{character}]\cr
#'   If not missing, where to save the returned splits as an ARFF file via \code{\link{write.arff}}.
#'   Default is no saving.
#' @return [\code{data.frame}]. Splits as defined in the algorithm benchmark repository
#'   specification text.
#'   Has columns: \dQuote{instance_id}, \dQuote{fold}, \dQuote{rep}.
#'   Defines which instances go into the test set for each replication / fold during CV.
#'   The training set are the remaining instances, in exactly the order as given by the data.frame
#'   for the current repetition.
#' @export
createCVSplits = function(astask, reps = 1L, folds = 10L, file = NULL) {
  checkArg(astask, "ASTask")
  reps = convertInteger(reps)
  checkArg(reps, "integer", len = 1L, na.ok = FALSE)
  folds = convertInteger(folds)
  checkArg(folds, "integer", len = 1L, na.ok = FALSE)
  requirePackages("mlr", "createCVSplits")
  if (!missing(file))
    checkArg(file, "character", len = 1L, na.ok = FALSE)

  instances = getInstanceNames(astask)
  size = length(instances)
  if (reps == 1L)
    rdesc = makeResampleDesc("CV", iters = folds)
  else
    rdesc = makeResampleDesc("RepCV", folds = folds, reps = reps)
  rin = makeResampleInstance(rdesc, size = size)
  splits = rin$test.inds
  splits = lapply(seq_along(splits), function(i)
    data.frame(instance_id = splits[[i]], fold = (i - 1) %% folds  + 1, rep = (i - 1) %/% folds + 1))
  splits = do.call(rbind, splits)
  splits$instance_id = instances[splits$instance_id]
  if (!missing(file))
    write.arff(splits, file)
  return(splits)
}




