#' Calculate aggregated performances per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param aggr [\code{function}]\cr
#'   Agregation function.
#'   Default is \code{mean}
#' @return [\code{data.frame}]. First column is algorithm names, further columns are aggregated performance values.
#' @export
#FIXME waht to aggregtae?
calcAggrPerfValues = function(astask, aggr = median) {
  checkArg(astask, "ASTask")
  checkArg(aggr, "function")

  ars = astask$algo.runs
  perfs = astask$desc$performance_measures
  res = ddply(ars, c("algorithm"), function(d) {
    p = d[, perfs, drop = FALSE]
    apply(p, 2, aggr)
  })
  res
}

#' Plot boxplots of performance values per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @return ggplot2 plot object.
#' @export
plotAlgoPerfBoxplots = function(astask, measure) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)

  data = astask$algo.runs
  p = ggplot(data, aes_string(x = "algorithm", y = measure)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle=90, vjust=1))
  p
}

#' Plot densities of performance values per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @return ggplot2 plot object.
#' @export
plotAlgoPerfDensities = function(astask, measure) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)

  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  data = astask$algo.runs
  p = ggplot(data, aes_string(x = measure, col = "algorithm")) +
    geom_density()
  p
}



