# aggregates stochastic algo perf by averaging
aggregateStochasticAlgoPerf = function(algo.perf, with.instance.id = TRUE) {
  # only do this if repeated measurements to save time
  if (max(algo.perf$repetition) > 1L) {
    algo.perf = ddply(algo.perf, c("instance_id", "algorithm"), function(d) {
      colMeans(d[, measure, drop = FALSE])
    })
  }
  drop = "repetition"
  if (!with.instance.id)
    drop = c(drop, "instance_id")
  dropNamed(algo.perf, drop)
}

