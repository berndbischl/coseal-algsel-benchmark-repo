#' Calculate aggregated performances per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param aggr [\code{function}]\cr
#'   Agregation function.
#'   Default is \code{mean}
#' @return [\code{data.frame}]. Rows correspond to algorithms and are named in this fashion.
#'   For each performance measure two columns are created: One for the aggregated performance values,
#'   one for the percentage of missing values.
#' @export

calcAggrPerfValues = function(astask, aggr) {
  checkArg(astask, "ASTask")
  if (missing(aggr))
    aggr = function(x) median(x, na.rm = TRUE)
  else
    checkArg(aggr, "function")

  ars = astask$algo.runs
  measures = astask$desc$performance_measures
  res = ddply(ars, c("algorithm"), function(d) {
    row = list()
    row = lapply(measures, function(m) {
      p = d[, m]
      setNames(
        c(aggr(p), nas = mean(is.na(p) * 100)),
        c(m, sprintf("%s.nas", m))
      )
    })
    as.data.frame(as.list(unlist(row)))
  })
  rownames(res) = res$algorithm
  res$algorithm = NULL
  return(res)
}

#' Plot boxplots of performance values per algorithm across all instances.
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
plotAlgoPerfBoxplots = function(astask, measure, log = FALSE) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  checkArg(log, "logical", len=1L, na.ok=FALSE)

  data = astask$algo.runs
  data = data[!is.na(data[,measure]), ]
  if (log) 
    data = data[(data[,measure] > 0), ]
  p = ggplot(data, aes_string(x = "algorithm", y = measure, col = "algorithm")) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle=90, vjust=1))
  if (log)  
    p = p + scale_y_log10()
  p
}

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

  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  data = astask$algo.runs
  data = data[!is.na(data[,measure]), ]
  if (log) 
    data = data[(data[,measure] > 0), ]
  p = ggplot(data, aes_string(x = measure, col = "algorithm")) +
    geom_density()
  if (log)  
    p = p + scale_x_log10()
  p
}



