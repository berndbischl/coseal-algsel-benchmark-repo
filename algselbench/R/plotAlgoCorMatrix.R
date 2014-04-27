#' @title Plots the correlation matrix of the algorithms.
#'
#' @description
#' If NAs occur, they are imputed (before aggregation) by
#' \code{base + 0.3 * range}.
#' \code{base} is the cutoff value for runtimes tasks with cutoff or
#' the worst performance for all others.
#'
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
#'   See \code{\link[corrplot]{corrMatOrder}}.
#'   Default is \dQuote{hclust}.
#' @param hclust.method [\code{character(1)}]\cr
#'   Method for hierarchical clustering. Only useful, when \code{order.method}
#'   is set to \dQuote{hclust}, otherwise ignored.
#'   Possible values are: \dQuote{ward}, \dQuote{single},
#'   \dQuote{complete}, \dQuote{average}, \dQuote{mcquitty}, \dQuote{median} and
#'   \dQuote{centroid}.
#'   See \code{\link[corrplot]{corrMatOrder}}.
#'   Default is \dQuote{ward}.
#' @param cor.method [\code{character(1)}]\cr
#'   Method to be used for calculating the correlation between the algorithms.
#'   Possible values are \dQuote{pearson}, \dQuote{kendall} and \dQuote{spearman}.
#'   See \code{\link{cor}}.
#'   Default is \dQuote{spearman}.
#' @return See \code{\link[corrplot]{corrplot}}.
#' @export
plotAlgoCorMatrix = function(astask, measure, order.method = "hclust", hclust.method = "ward",
  cor.method = "spearman") {

  checkArg(order.method, choices = c("hclust", "FPC", "AOE", "original", "alphabet"))
  checkArg(hclust.method, choices =
    c("ward", "single", "complete", "average", "mcquitty", "median", "centroid"))
  checkArg(cor.method, choices = c("pearson", "kendall", "spearman"))

  z = getEDAAlgoPerf(astask, measure, impute.failed.runs = TRUE, jitter = FALSE,
    impute.zero.vals = FALSE, check.log = FALSE, format = "wide", with.instance.id = FALSE)

  cor.matrix = cor(z$data, method = cor.method)
  ind = corrMatOrder(cor.matrix, order = order.method, hclust.method = hclust.method)
  corrplot(cor.matrix[ind, ind], method = "shade")
}
