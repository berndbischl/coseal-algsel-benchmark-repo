# throws an exception if one tries to calculate the logarithm of non-negative values
checkLogarithm = function(log, x, col) {
  if (log) {
    if (!missing(col))
      x = x$col
    if ((any(x <= 0)))
      stop("Cannot compute logarithm of <= 0 value! ")
  }
}
