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

