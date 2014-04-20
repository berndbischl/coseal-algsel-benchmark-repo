#' Creates a scatterplot matrix of the performance values of the algorithms.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param trafo [\code{function(1)}]\cr
#'   Function applied to the data as preprocessing to the generation of the plot.
#'   Default is identity.
#' @return  plot object.
#' @export
plotAlgoScatterMatrix = function(astask, measure, trafo = identity) {
  algo.perf = getEDAAlgoPerf(astask, measure)
  ggpairs(z$algo.perf,
    lower = list(continuous = "smooth"),
    diag = list(continuous = "blank"),
    upper = list(continuous = "cor")
  )
}

