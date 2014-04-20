#' Plots the correlation matrix of the algorithms.
#'
#' If NAs occur, they are imputed (before aggregation) either by 10 * cutoff
#' (for runtimes tasks with cutoff) or 10 * <worst performance> for all others.
#' Stochastic replications are aggregated by the mean value.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param order.method [\code{character(1)}]\cr
#'   Method for ordering the algorithms within the plot.
#'   Possible values are \dQuote{hclust} (for hierarchical clustering order),
#'   \dQuote{FPC} (first principal component order), \dQuote{AOE} (angular order
#'   of eigenvectors), \dQuote{original} (original order) and \dQuote{alphabet}
#'   (alphabetical order).
#'   See \code{\link[corrplot]{corMatOrder}}.
#'   Default is \dQuote{hclust}.
#' @param hclust.method [\code{character(1)}]\cr
#'   Method for hierarchical clustering. Only useful, when \code{order.method}
#'   is set to \dQuote{hclust}, otherwise ignored.
#'   Possible values are: \dQuote{ward}, \dQuote{single},
#'   \dQuote{complete}, \dQuote{average}, \dQuote{mcquitty}, \dQuote{median} and
#'   \dQuote{centroid}.
#'   See \code{\link[corrplot]{corMatOrder}}.
#'   Default is \dQuote{ward}.
#' @param cor.method [\code{character(1)}]\cr
#'   Method to be used for calculating the correlation between the algorithms.
#'   Possible values are \dQuote{pearson}, \dQuote{kendall} and \dQuote{spearman}.
#'   See \code{\link{cor}}.
#'   Default is \dQuote{spearman}.
#' @return corrplot
#' @export
plotAlgoCorMatrix = function(astask, measure, order.method = "hclust", hclust.method = "ward",
  cor.method = "spearman") {

  requirePackages("corrplot", why = "plotAlgoCorMatrix", quietly = TRUE)
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, choices = astask$desc$performance_measures)
  checkArg(order.method, choices = c("hclust", "FPC", "AOE", "original", "alphabet"))
  checkArg(hclust.method, choices =
    c("ward", "single", "complete", "average", "mcquitty", "median", "centroid"))
  checkArg(cor.method, choices = c("pearson", "kendall", "spearman"))

  algo.perf = imputeAlgoPerf(astask, measure, )
  algo.perf = aggregateStochasticAlgoPerf(algo.perf, with.instance.id = FALSE)

  cor.matrix = cor(algo.perf, method = cor.method)
  ind = corrMatOrder(cor.matrix, order = order.method, hclust.method = hclust.method)
  corrplot(cor.matrix[ind, ind], method = "shade")
}
