#' Creates a data.frame that summarizes the feature steps.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}].
#' @export
summarizeFeatureSteps= function(astask) {
  checkArg(astask, "ASTask")
  rs = astask$feature.runstatus
  # First 2 cols are instanceid and rep
  # create prop table for the subsequent step columns over their levels
  n = ncol(rs)
  res = dapply(rs[, 3:n, drop=FALSE], function(x)
    100 * prop.table(table(x)))
  # add size of step = number of features in it
  size = sapply(astask$desc$feature_steps, length)
  res = setColNames(t(res), levels(rs[, 3]))
  res = cbind(size = size, res)
  fc = astask$feature.costs
  if (!is.null(fc)) {
    res2 = t(sapply(fc[, 3:n, drop=FALSE], function(x) c(
      cost.min = min(x, na.rm = TRUE),
      cost.mean = mean(x, na.rm = TRUE),
      cost.max = max(x, na.rm = TRUE),
      cost.na = mean(is.na(x))
    )))
    res = cbind(res, res2)
  }
  return(res)
}
