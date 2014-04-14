#' Plot cumulative distribution function of performance values per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param log [\code{logical(1)}]\cr
#'   Should the performance values be log10-transformed in the plot?
#'   Default is FALSE.
#' @param na.impute [\code{logical(1)}]\cr
#'   Should the values of algorithm runs with non-ok runstatus (missing performance 
#'   values) be imputed? If yes, imputation is done via max + scalar * (max - min) for
#'   minimization problems and via min - scalar * (max - min) for maximization problems.
#'   Default is TRUE.
#' @return ggplot2 plot object.
#' @export
plotAlgoPerfProbability = function(astask, measure, log = FALSE, na.impute = TRUE) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  checkArg(log, "logical", len=1L, na.ok=FALSE)
  checkArg(na.impute, "logical", len=1L, na.ok=FALSE)
  
  if (na.impute)
    astask = imputeCrashedRuns(astask)
  data = astask$algo.runs
  data = data[!is.na(data[,measure]), ]
  if (log)
    data = data[(data[,measure] > 0), ]
  p = ggplot(data, aes_string(x = measure, col = "algorithm")) +
    stat_ecdf()
  if (log)
    p = p + scale_x_log10()
  p
}



