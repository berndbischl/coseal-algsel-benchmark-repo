imputeZeroVals = function(algo.runs, measure, impute.zero.vals) {
  if (impute.zero.vals) {
    p = algo.runs[, measure]
    is0 = !is.na(p) & p == 0
    algo.runs[is0, measure] = 1e-6
  }
  return(algo.runs)
}
