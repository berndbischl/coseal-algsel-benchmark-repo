# mainly for internal use
#  - selects algo.perf matrix
#  - imputes it (in standard way)
#  - aggregates replications


getEDADataCheck = function(astask, measure) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, choices = astask$desc$performance_measures)
  return(measure)
}


getEDAAlgoPerf = function(astask, measure) {
  measure = getEDAAlgoRuns(astask, measure)
  ap = imputeAlgoPerf(astask, measure = measure, structure = "algo.perf")
  ap = aggregateStochasticAlgoPerf(algo.perf = ap, with.instance.id = FALSE)
  #FIXME: check log
  list(data = data, measure = measure)
}

getEDAAlgoRuns = function(astask, measure) {
  measure = getEDAAlgoRuns(astask, measure)
  ar = imputeAlgoPerf(astask, measure = measure, structure = "algo.runs")
  algo.perf = aggregateStochasticAlgoPerf(algo.perf, with.instance.id = FALSE)
  checkLogarithm(data[, measure], log)
  list(data = data, measure = measure)
}

