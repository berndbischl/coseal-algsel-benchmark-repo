#' Checks algorithm and feature data sets for useless instances.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param measure [\code{character(1)}]\cr
#'   Measure that's been used for analyzing the algorithm performances.
#'   Default is first measure in scenario.
#' @return [\code{list}]. List of data frames, which give an overview of instances that 
#'  consists of only one single value (and eventually some NAs) per instance.
#'  The output consists of three data frames: one for the algo runs, the feature values and the
#'  feature runstatus, respectively.
#' @export
findUselessInstances = function(asscenario, measure) {
  checkArg(asscenario, "ASScenario")
  if (missing(measure))
    measure = asscenario$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  algo.performances = asscenario$algo.runs
  feat.vals = asscenario$feature.values
  feat.runs = asscenario$feature.runstatus
  splitted.algos = split(algo.performances[[measure]], 
                         algo.performances$algorithm)
  checked.algos = findUniqueValues(splitted.algos)
  checked.algos = checked.algos[
    (checked.algos$uniqueValue != -Inf) | (checked.algos$NAs != -Inf), ]
  colnames(checked.algos) = c("unique Value(s)", "NA's")
  checked.feat.vals = findUniqueValues(feat.vals)[-(1:2),]
  checked.feat.vals = checked.feat.vals[
    (checked.feat.vals$uniqueValue != -Inf) | (checked.feat.vals$NAs != -Inf), ]
  colnames(checked.feat.vals) = c("unique Value(s)", "NA's")
  checked.feat.runs = findUniqueValues(feat.runs)[-(1:2),]
  checked.feat.runs = checked.feat.runs[
    (checked.feat.runs$uniqueValue != -Inf) | (checked.feat.runs$NAs != -Inf), ]
  checked.feat.runs = checked.feat.runs[
    (as.character(checked.feat.runs$uniqueValue) != "ok") | (checked.feat.runs$NAs != 0), ]
  colnames(checked.feat.runs) = c("unique Value(s)", "NA's")
  return(list(algo.runs = checked.algos, feature.values = checked.feat.vals, 
              feature.runstatus = checked.feat.runs))
}


## Helper function that checks whether there are features/algorithms
## that consist of only one single value and/or NA's.
findUniqueValues = function(data) {
  result = t(sapply(data, function(x) {
    if (is.factor(x)) 
      x = as.character(x)
    NAs = is.na(x)
    without.NAs = x[!NAs]
    if (length(unique(without.NAs)) <= 1) {
      unique.values = unique(without.NAs)
      if (length(unique.values) == 0) 
        unique.values = NA
      return(c(uniqueValue = unique.values, NAs = sum(NAs)))
    } else {
      return(c(uniqueValue = -Inf, NAs = -Inf))
    }
  }))
  return(as.data.frame(result))
}
