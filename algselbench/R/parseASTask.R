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
  feature.values = read.arff(file.path(path, "feature_values.arff"))
  algo.runs = read.arff(file.path(path, "algorithm_runs.arff"))

  makeS3Obj("ASTask",
    desc = desc,
    feature.runstatus = feature.runstatus,
    feature.values = feature.values,
    algo.runs = algo.runs
  )
}

#' @S3method print ASTask
print.ASTask = function(x, ...) {
  print(x$desc)
}

