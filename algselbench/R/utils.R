checkMeasure = function(measure, desc) {
  if (missing(measure))
    measure = desc$performance_measures[1]
  else
    checkArg(measure, choices = desc$performance_measures)
  return(measure)
}

# throws an exception if one tries to calculate the logarithm of non-negative values
checkLogarithm = function(log, x, col) {
  if (log) {
    if (!missing(col))
      x = x[, col]
    if ((any(x <= 0)))
      stop("Cannot compute logarithm of <= 0 value! ")
  }
}

# put vector in, get vector of stats out
getStatistics = function(x) {
  # we drop name for quantiles
  funs = list(
    obs = function(x) length(x),
    nas = function(x) sum(is.na(x)),
    min = function(x) min(x, na.rm = TRUE),
    qu_1st = function(x) as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    med = function(x) median(x, na.rm = TRUE),
    mean = function(x) mean(x, na.rm = TRUE),
    qu_3rd  = function(x) as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    max = function(x) max(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE),
    coeff_var = function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  )
  sapply(funs, function(f) f(x))
}
