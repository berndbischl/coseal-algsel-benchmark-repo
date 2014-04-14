imputeCrashedRuns = function(astask, scalar = 0.3) {
  data = astask$algo.runs
  cns = colnames(data)
  measures = setdiff(cns, c("instance_id", "repetition", "algorithm", "runstatus"))
  crashed = (data$runstatus != "ok")
  if (astask$desc$maximize) {
    data[crashed, measures] = sapply(measures, function(meas) {
      perf.range = range(data[!crashed, meas], na.rm = TRUE)
      noise = rnorm(sum(crashed), sd = 0.05 * diff(perf.range))
      return(perf.range[1] - scalar * diff(perf.range) + noise)
    })
  } else {
    data[crashed, measures] = sapply(measures, function(meas) {
      perf.range = range(data[!crashed, meas], na.rm = TRUE)
      noise = rnorm(sum(crashed), sd = 0.05 * diff(perf.range))
      return((perf.range[2] + scalar * diff(perf.range)) + noise)
    })
  }
  astask$algo.runs = data
  return(astask)
}
