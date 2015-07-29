#' @title Writes an algorithm selection scenario to a directory.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param path [\code{character(1)}]\cr
#'   Path to write scenario to. Default is the name of the scenario.
#' @export
#' @aliases ASScenario
writeASScenario = function(asscenario, path = asscenario$desc$scenario_id) {
  assertClass(asscenario, "ASScenario")

  dir.create(path)
  oldwd = getwd()
  setwd(path)

  desc = lapply(asscenario$desc, yamlify)
  cat(as.yaml(desc), file = "description.txt")

  write.arff(asscenario$feature.values, "feature_values.arff")
  write.arff(asscenario$feature.runstatus, "feature_runstatus.arff")
  if (!is.null(asscenario$feature.costs)) {
    write.arff(asscenario$feature.costs, "feature_costs.arff")
  }

  if (!is.null(asscenario$algo.runs)) {
    write.arff(asscenario$algo.runs, "algorithm_runs.arff")
  }

  if (!is.null(asscenario$cv.splits)) {
    write.arff(asscenario$cv.splits, "cv.arff")
  }

  setwd(oldwd)
}

yamlify = function(thing) {
  if (length(thing) == 1L && is.na(thing)) {
    "?"
  } else {
    thing
  }
}
