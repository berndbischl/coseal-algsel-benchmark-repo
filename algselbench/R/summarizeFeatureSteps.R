#' Creates a data.frame that summarizes the feature steps.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{data.frame}].
#' @export
summarizeFeatureSteps= function(asscenario) {
  assertClass(asscenario, "ASScenario")
  rs = asscenario$feature.runstatus
  # First 2 cols are instanceid and rep
  # create prop table for the subsequent step columns over their levels
  n = ncol(rs)
  res = sapply(rs[, 3:n, drop = FALSE], function(x)
    100 * as.numeric(prop.table(table(x))))
  # add size of step = number of features in it
  size = sapply(asscenario$desc$feature_steps, length)
  res = as.data.frame(setColNames(t(res), levels(rs[, 3])))
  res = cbind(size = size, res)
  fc = asscenario$feature.costs
  if (!is.null(fc)) {
    res2 = t(sapply(fc[, 3:n, drop = FALSE], function(x) c(
      cost_min = min(x, na.rm = TRUE),
      cost_mean = mean(x, na.rm = TRUE),
      cost_max = max(x, na.rm = TRUE),
      cost_na = mean(is.na(x))
    )))
    res = cbind(res, res2)
  }
  return(res)
}
