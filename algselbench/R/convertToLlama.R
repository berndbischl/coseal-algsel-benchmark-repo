#' Convert an ASTask task object to a llama data object.
#'
#' For stochastic algorithms and features, mean values are computed over the repetitions.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to use for modelling.
#'   Default is first measure in task.
#' @return Result of calling \code{\link[llama]{input}}.
#' @export
convertToLlama = function(astask, measure) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  
  feats = astask$feature.values
  # aggregate stochastic features, only do this if repeated to save time
  if (max(feats$repetition) > 1L) {
    feats = ddply(feats, c("instance_id"), function(d) {
      colMeans(d[, getFeatureNames(astask), drop = FALSE])
    })
  }

  # convert to perf to wide format
  perf = astask$algo.runs
  #FIXME: what do we do, if runstatus is not ok? impute?
  # drop unneeded stuff and average over repetitions
  perf = subset(perf, select = c("instance_id", "repetition", "algorithm", measure, "runstatus"))
  # aggregate stochastic algorithms, only do this if repeated to save time
  if (max(perf$repetition) > 1L) {
    perf = ddply(perf, c("instance_id", "algorithm"), function(d) {
      colMeans(d[, measure, drop = FALSE])
    })
  }
  # wide format
  perf = dcast(perf, instance_id ~ algorithm, value.var = measure)
  # impute NAs
  # FIXME:
  # FIXME: llama does not work if NAs are in data, should be checked
  perf[is.na(perf)] = 10

  # conctruct solved
  # FIXME: 
  successes = as.data.frame(matrix(TRUE, nrow = nrow(perf), ncol = ncol(perf)))
  colnames(successes) = colnames(perf)
  successes$instance_id = perf$instance_id 
  input(feats, perf, successes = successes, minimize = !astask$desc$maximize[[measure]])
}

