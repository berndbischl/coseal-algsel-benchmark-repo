# converts the large algo.run in ASTask to a handier data.frame where rows are instances, col are algos
convertAlgoTunsToAlgoPerf = function(desc, algo.runs, measure) {
  ap = dcast(algo.runs, instance_id + repetition ~ algorithm, value.var = measure)
  # sort rows and cols
  ap = ap[, c("instance_id", "repetition",
    desc$algorithms_deterministic, desc$algorithms_stochastic)]
  sortByCol(ap, c("instance_id", "repetition"))
}
