#' Convert an ASTask task object to a llama data object.
#'
#' For stochastic algorithms and features, mean values are computed across repetitions.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to use for modeling.
#'   Default is first measure in task.
#' @param feature.steps [\code{character}]\cr
#'   Which feature steps are allowed?
#'   Default is all steps.
#' @param add.feature.costs [\code{logical(1)}]\cr
#'   If costs for features are present in runtime tasks, should they be added to the algorithm costs
#'   (because in reality you would  have to pay them)? Whether the algorithm hit the cutoff runtime
#'   is also newly calculated in this case.
#'   This adding of feature costs should not be done for the
#'   baseline models, but only for proper prognostic models.
#'   If no costs are present, 0 is added as costs and a warning is issued.
#'   Default is \code{TRUE}.
#' @return Result of calling \code{\link[llama]{input}}.
#' @export
convertToLlama = function(astask, measure, feature.steps, add.feature.costs = TRUE) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  allsteps = names(astask$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = allsteps
  else
    checkArg(feature.steps, subset = allsteps)
  checkArg(add.feature.costs, "logical", len = 1L, na.ok = FALSE)
  if (add.feature.costs && is.null(astask$feature.costs))
    warningf("Requested to add feature costs, but none in task. Adding always 0 feature costs.")

  desc = astask$desc
  feats = astask$feature.values
  allowed.features = getProvidedFeatures(astask, feature.steps)

  ### handle features ###

  # reduce to inst + rep + allowed features
  feats = feats[, c("instance_id", "repetition", allowed.features), drop = FALSE]

  # aggregate stochastic features, only do this if repeated measurements to save time
  if (max(feats$repetition) > 1L) {
    feats = ddply(feats, c("instance_id"), function(d) {
      colMeans(d[, allowed.features, drop = FALSE])
    })
  } else {
    feats$repetition = NULL
  }
  #FIXME: maybe use a backup solver when some features are NA, instead of imputing

  # impute feature values
  # FIXME: why cant we impute without target
  # FIXME: check whether imputing the median is useful
  cols = sapply(feats, function(x) any(is.na(x)))
  cols = names(cols)[cols]
  cols = setNames(lapply(cols, function(x) imputeMedian()), cols)
  feats = impute(feats, target = character(0), cols = cols)$data

  ### handle perf values ###

  perf = astask$algo.runs
  perf = subset(perf, select = c("instance_id", "repetition", "algorithm", measure, "runstatus"))

  # wide format
  runstatus = dcast(perf, instance_id + repetition ~ algorithm, value.var = "runstatus")
  perf = dcast(perf, instance_id + repetition ~ algorithm, value.var = measure)
  cols = 3:ncol(perf)
  runstatus2 = runstatus[, cols]
  perf2 = perf[, cols]

  # construct successes, so far means: no NA in perf val and run status of algo is "OK"
  cutoff = desc$algorithm_cutoff_time
  successes = !is.na(perf2) & runstatus2 == "ok"

  presolve = getCostsAndPresolvedStatus(astask, feature.steps = feature.steps)

  # impute performance values and add feature costs for run time tasks
  if (desc$performance_type[measure] == "runtime" & !is.na(cutoff)) {
    impute.val = desc$algorithm_cutoff_time
    if (add.feature.costs) {
      m = ncol(perf2)
      # set algorithm costs to 0 for presolved instances, they wont run
      perf2[presolve$is.presolved, ] = 0
      if (is.null(astask$feature.costs)) 
        add = 0
      else
        add = matrix(rep(presolve$costs, m), ncol = m, byrow = FALSE)
      # add instance costs (adapated by presolving) to each alg column
      perf2 = perf2 + add    
    }
    # recalculate successes wrt to new perf vals and cutoff. we spent more time due to feat costs
    successes = successes & perf2 <= cutoff
  } else {
    impute.val = 10 * max(perf2, na.rm = TRUE)
  }
  perf2[!successes] = impute.val
  # copy back to perf object
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
  input(feats, perf, successes = successes,
    minimize = !astask$desc$maximize[[which(astask$desc$performance_measures == measure)]])
}

