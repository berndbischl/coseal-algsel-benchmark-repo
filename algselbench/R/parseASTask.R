#' Parses description file and returns an S3 class of the contents
#'
#' @param path [\code{character(1)}]\cr
#'   Path to directory of benchmark data set.
#' @return [\code{\link{ASTask}}]. Description object.
#' @export
#' @aliases ASTask
parseASTask = function(path) {
  checkArg(path, "character", len = 1L, na.ok = FALSE)

  desc = parseDescription(path)
  feature.runstatus = read.arff(file.path(path, "feature_runstatus.arff"))
  # make sure we have correct levels
  for (j in 3:ncol(feature.runstatus)) {
    feature.runstatus[, j] = factor(feature.runstatus[, j],
      levels = c("ok", "timeout", "memout", "presolved", "crash", "other"))
  }
  feature.values = read.arff(file.path(path, "feature_values.arff"))
  costfile = file.path(path, "feature_costs.arff")
  feature.costs = if (file.exists(costfile))
    read.arff(file.path(path, "feature_costs.arff"))
  else
    NULL
  algo.runs = read.arff(file.path(path, "algorithm_runs.arff"))
  cv.splits= read.arff(file.path(path, "cv.arff"))

  makeS3Obj("ASTask",
    desc = desc,
    feature.runstatus = feature.runstatus,
    feature.values = feature.values,
    feature.costs= feature.costs,
    algo.runs = algo.runs,
    cv.splits = cv.splits
  )
}

#' @S3method print ASTask
print.ASTask = function(x, ...) {
  d = x$desc
  printField1 = function(name, val) {
    k = length(val)
    if (k == 0L)
      catf("%-30s        : -", name)
    else if (k == 1L)
      catf("%-30s        : %s", name, clipString(as.character(val), 60L))
    else
      catf("%-30s (%3i)  : %s", name, k, clipString(collapse(val, sep = ", "), 60L))
  }
  x$feature_steps = names(x$feature_steps)
  printField1("Task id", d$task_id)
  printField1("Performance measures", d$performance_measures)
  printField1("Performance types", d$performance_type)
  printField1("Algorithm cutoff time", d$algorithm_cutoff_time)
  printField1("Algorithm cutoff mem", d$algorithm_cutoff_memory)
  printField1("Feature cutoff time", d$features_cutoff_time)
  printField1("Feature cutoff mem", d$features_cutoff_memory)
  printField1("Nr. of instances", length(unique(x$feature.values$instance_id)))
  printField1("Features (deterministic)", d$features_deterministic)
  printField1("Features (stochastic)", d$features_stochastic)
  printField1("Feature repetitions", collapse(range(x$feature.values$repetition), sep = " - "))
  printField1("Feature costs", ifelse(is.null(x$feature.costs), "No", "Yes"))
  printField1("Algo. (deterministic)", d$algorithms_deterministic)
  printField1("Algo. (stochastic)", d$algorithms_stochastic)
  printField1("Algo. repetitions", collapse(range(x$algo.runs$repetition), sep = " - "))
  printField1("Algo. runs (inst x algo x rep)", nrow(x$algo.runs))
  printField1("Nr. of feature steps", d$number_of_feature_steps)
  printField1("CV repetitions", getNumberOfCVReps(x))
  printField1("CV folds", getNumberOfCVFolds(x))
}

