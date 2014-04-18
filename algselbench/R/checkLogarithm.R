# throws an exception if one tries to calculate the logarithm of non-negative values
checkLogarithm = function(x, log) {
  if (log && (any(x <= 0)))
    stop("Can't compute logarithm of non-negative values.")
}