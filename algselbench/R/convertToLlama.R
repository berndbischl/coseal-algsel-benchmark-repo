#' Convert an ASTask task object to a llama data object.
#'
#' For stochastic algorithms and features, mean values are computed across repetitions.
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

  desc = astask$desc
  feats = astask$feature.values

  ### handle features ###

  # aggregate stochastic features, only do this if repeated measurements to save time
  if (max(feats$repetition) > 1L) {
    feats = ddply(feats, c("instance_id"), function(d) {
      colMeans(d[, getFeatureNames(astask), drop = FALSE])
    })
  } else {
    feats$repetition = NULL
  }

  # impute feature values
  # FIXME: why cant we impute without target
  feats$.y = 1
  feats = impute(feats, target = ".y")$data
  feats$.y =  NULL

  ### handle perf values ###

  perf = astask$algo.runs
  perf = subset(perf, select = c("instance_id", "repetition", "algorithm", measure, "runstatus"))

  # wide format
  runstatus = dcast(perf, instance_id + repetition ~ algorithm, value.var = "runstatus")
  perf = dcast(perf, instance_id + repetition ~ algorithm, value.var = measure)
  cols = 3:ncol(perf)
  runstatus2 = runstatus[, cols]
  perf2 = perf[, cols]

  # construct successes
  successes = !is.na(perf2) & runstatus2 == "ok"

  # impute performance values
  if (desc$performance_type[measure] == "runtime" & !is.na(desc$algorithm_cutoff_time))
    impute.val = desc$algorithm_cutoff_time
  else
    impute.val = 10 * max(perf2, na.rm = TRUE)
  perf2[!successes] = impute.val
  perf[, cols] = perf2

  # aggregate stochastic algorithms, only do this if repeated measurements to save time
  if (max(perf$repetition) > 1L) {
    perf = ddply(perf, c("instance_id", "algorithm"), function(d) {
      colMeans(d[, measure, drop = FALSE])
    })
  } else {
    perf$repetition = NULL
  }

  # add instance_id to successes then call llama
  successes = as.data.frame(successes)
  successes = cbind(instance_id = perf$instance_id, successes)
  colnames(successes) = colnames(perf)
  input(feats, perf, successes = successes, minimize = !astask$desc$maximize[[measure]])
}

