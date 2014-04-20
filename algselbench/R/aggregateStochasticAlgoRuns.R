# aggregates stochastic algo perf by averaging, drops other columns
aggregateStochasticAlgoRuns = function(algo.runs, measure, with.repetition = FALSE) {
  # only do this if repeated measurements to save time
  if (max(algo.runs$repetition) > 1L) {
    algo.runs = ddply(algo.runs, c("instance_id", "algorithm"), function(d) {
      e = d[1L, ]
      e[, measure] = mean(d[, measure])
      return(e)
    })
  }

  if (with.repetition) {
    algo.runs$repetition = 1L
    cols = c("instance_id", "repetition", "algorithm", measure)
  } else {
    cols = c("instance_id", "algorithm", measure)
  }
  return(algo.runs[, cols])
}


