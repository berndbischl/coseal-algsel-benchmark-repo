#' @title Plots the correlation matrix of the algorithms.
#'
#' @description
#' If NAs occur, they are imputed (before aggregation) by
#' \code{base + 0.3 * range}.
#' \code{base} is the cutoff value for runtimes scenarios with cutoff or
#' the worst performance for all others.
#'
#' Stochastic replications are aggregated by the mean value.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in scenario.
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
#'   Possible values are: \dQuote{ward.D2}, \dQuote{single},
#'   \dQuote{complete}, \dQuote{average}, \dQuote{mcquitty}, \dQuote{median} and
#'   \dQuote{centroid}.
#'   See \code{\link[corrplot]{corrMatOrder}}.
#'   Default is \dQuote{ward.D2}.
#' @param cor.method [\code{character(1)}]\cr
#'   Method to be used for calculating the correlation between the algorithms.
#'   Possible values are \dQuote{pearson}, \dQuote{kendall} and \dQuote{spearman}.
#'   See \code{\link{cor}}.
#'   Default is \dQuote{spearman}.
#' @return See \code{\link[corrplot]{corrplot}}.
#' @export
plotAlgoCorMatrix = function(asscenario, measure, order.method = "hclust", hclust.method = "ward.D2",
  cor.method = "spearman") {

  assertChoice(order.method, choices = c("hclust", "FPC", "AOE", "original", "alphabet"))
  assertChoice(hclust.method, choices =
    c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"))
  assertChoice(cor.method, choices = c("pearson", "kendall", "spearman"))

  z = getEDAAlgoPerf(asscenario, measure, impute.failed.runs = TRUE, jitter = FALSE,
    impute.zero.vals = FALSE, check.log = FALSE, format = "wide", with.instance.id = FALSE)

  cor.matrix = cor(z$data, method = cor.method)
  ind = aslibCorrMatOrder(cor.matrix, order = order.method, hclust.method = hclust.method)
  corrplot(cor.matrix[ind, ind], method = "shade")
}

## helper function, which is basically identical to corrplot::corrMatOrder, but already
## uses "ward.D" and "ward.D2" instead of the old "ward"
aslibCorrMatOrder = function (corr, order = c("AOE", "FPC", "hclust", "alphabet"), 
  hclust.method = c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid")) {

  order = match.arg(order)
  hclust.method = match.arg(hclust.method)
  if (order == "AOE") {
    x.eigen = eigen(corr)$vectors[, 1:2]
    e1 = x.eigen[, 1]
    e2 = x.eigen[, 2]
    alpha = ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
    ord = order(alpha)
  }
  if (order == "FPC") {
    x.eigen = eigen(corr)$vectors[, 1:2]
    e1 = x.eigen[, 1]
    ord = order(e1)
  }
  if (order == "alphabet") {
    ord = sort(rownames(corr))
  }
  if (order == "hclust") {
    ord = order.dendrogram(as.dendrogram(hclust(as.dist(1 - corr), method = hclust.method)))
  }
  return(ord)
}
