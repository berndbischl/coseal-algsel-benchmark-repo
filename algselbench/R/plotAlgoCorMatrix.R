#' Plots the correlation matrix of the algorithms.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param order.method [\code{character(1)}]\cr
#'   Method for ordering the algorithms within the plot.
#'   Possible values are \code{hclust} (for hierarchical clustering order),
#'   \code{FPC} (first principal component order), \code{AOE} (angular order
#'   of eigenvectors), \code{original} (original order) and \code{alphabet}
#'   (alphabetical order). Default is \code{hclust}.
#' @param hclust.method [\code{character(1)}]\cr
#'   Method for hierarchical clustering. Only useful, when \code{order.method} 
#'   is set to \code{hclust}. Possible values are \code{ward}, \code{single},
#'   \code{complete}, \code{average}, \code{mcquitty}, \code{median} and
#'   \code{centroid}.
#'   Default is \code{ward}.
#' @param cor.method [\code{character(1)}]\cr
#'   Method to be used for calculating the correlation between the algorithms.
#'   Possible values are \code{pearson}, \code{kendall} and \code{spearman}.
#'   The default is \code{pearson}.
#' @return corrplot
#' @export
plotAlgoCorMatrix = function(astask, measure, order.method, hclust.method, cor.method) {
  requirePackages("corrplot", why = "plotAlgoCorMatrix", quietly = TRUE)
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  if (missing(order.method))
    order.method = "hclust"
  else
    checkArg(order.method, "character", len = 1L, na.ok = FALSE)
  if (missing(hclust.method)) {
    if (order.method == "hclust")
      hclust.method = "ward"
  } else {
    if (order.method == "hclust")
      checkArg(hclust.method, "character", len = 1L, na.ok = FALSE)
    else {
      message("There's no reason to provide a clustering type, when you don't use 'hclust' for sorting the algorithms.")
      hclust.method = NA      
    }
  }
  if (missing(cor.method))
    cor.method = "pearson"
  else
    checkArg(cor.method, choices = c("pearson", "kendall", "spearman"))
  
  algo.perf = astask$algo.runs
  algos = unique(algo.perf$algorithm)
  perf = algo.perf[order(algo.perf[, "instance_id"], algo.perf[, "repetition"],
    algo.perf[, "algorithm"]), measure]
  data = matrix(perf, ncol = length(algos), byrow = TRUE)
  colnames(data) = sort(algos)
  data = apply(data, 2, function(x) rank(x, na.last = "keep"))
  suppressWarnings((cor.matrix = cor(data, use = "pairwise.complete.obs", 
    method = cor.method)))
  if (any(is.na(cor.matrix))) {
    row.index = which(apply(cor.matrix, 1, function(x) any(is.na(x))))[1]
    col.index = which(is.na(cor.matrix[row.index,]))[2]
    warning.message = sprintf("The correlation matrix might be wrong, because some of the pairwise correlations, e.g. the correlation between %s and %s, could not be computed properly.",
      colnames(data)[row.index], colnames(data)[col.index])
    warning(warning.message)
    cov.matrix = cov(data, use = "pairwise.complete.obs", method = cor.method)
    cor.matrix = cov2cor(cov.matrix)
  }
  if (!is.na(hclust.method)) {
    ind = corrMatOrder(cor.matrix, order = order.method, 
      hclust.method = hclust.method)    
  } else {
    ind = corrMatOrder(cor.matrix, order = order.method)
  }
  corrplot(cor.matrix[ind, ind], method = "shade")
}