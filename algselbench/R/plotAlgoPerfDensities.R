#' Plot densities of performance values per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param log [\code{logical(1)}]\cr
#'   Should the performance values be log10-transformed in the plot?
#'   Default is FALSE.
#' @return ggplot2 plot object.
#' @export
plotAlgoPerfDensities = function(astask, measure, log = FALSE) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  checkArg(log, "logical", len=1L, na.ok=FALSE)
  
  data = imputeAlgoPerf(astask, measure, structure = "algo.runs", jitter = 0.05)
  data = data[,setdiff(colnames(data), c("instance_id", "repetition"))]
  if (log)
    data = data[(data[,measure] > 0), ]
  p = ggplot(data, aes_string(x = measure, col = "algorithm")) +
  geom_density()
  if (log)
    p = p + scale_x_log10()
  p
}



