# throws an exception if one tries to calculate the logarithm of non-negative values
checkLogarithm = function(x, log) {
  if (log && (any(x <= 0)))
    stop("Cannot compute logarithm of <= 0 value! ")
}
