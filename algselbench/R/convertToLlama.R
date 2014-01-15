#' Convert an ASTask task object to a llama data object.
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
  # aggregate stochastic features
  # FIXME: is averaging ok?
  feats = ddply(feats, c("instance_id"), function(d) {
    colMeans(d[, getFeatureNames(astask), drop = FALSE])
  })

  # convert to perf to wide format
  perf = astask$algo.runs
  #FIXME what do we do, if runstatus is not ok? impute?
  # drop unneeded stuff and average over repetitions
  # FIXME: is averaging ok?
  perf = subset(perf, select = c("instance_id", "repetition", "algorithm", measure, "runstatus"))
  perf = ddply(perf, c("instance_id", "algorithm"), function(d) {
    colMeans(d[, measure, drop = FALSE])
  })
  # wide format
  perf = dcast(perf, instance_id ~ algorithm, value.var = measure)
  # impute NAs
  # FIXME:
  # FIXME: llama does not work if NAs in data, should be checked
  perf[is.na(perf)] = 10

  # conctruct solved
  # FIXME: 
  successes = matrix(TRUE, nrow = nrow(perf), ncol = ncol(perf))

  input(feats, perf, successes = successes, minimize = !astask$desc$maximize[[measure]])
}

