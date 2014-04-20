# aggregates stochastic algo perf by averaging, drops other columns
aggregateStochasticAlgoRuns = function(algo.runs, measure) {
  # only do this if repeated measurements to save time
  if (max(algo.runs$repetition) > 1L) {
    algo.runs = ddply(algo.runs, c("instance_id", "algorithm"), function(d) {
      e = d[1L, ]
      e[, measure] = mean(d[, measure])
      return(e)
    })
  }
  return(algo.runs[, c("instance_id", "algorithm", measure)])
}

# aggregates stochastic algo perf by averaging
aggregateStochasticAlgoPerf = function(algo.perf, with.instance.id = TRUE) {
  ar = aggregateStochasticAlgoPerf(algo.runs, measure)
  ap = convertAlgoTunsToAlgoPerf(ar)
  return(ap)

  # drop = "repetition"
  # if (!with.instance.id)
    # drop = c(drop, "instance_id")
  # dropNamed(algo.perf, drop)
}

