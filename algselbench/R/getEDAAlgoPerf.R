# helper to retrieve algo data for plotting
#  - imputes it (in standard way)
#  - aggregates replications
#  - returns in wide or long format


getEDAAlgoPerf = function(astask, measure, jitter, impute.zero.vals, check.log, format, with.instance.id) {
  checkArg(astask, "ASTask")
  desc = astask$desc
  measure = checkMeasure(measure, desc)
  checkArg(jitter, "logical", len = 1L, na.ok = FALSE)
  checkArg(impute.zero.vals, "logical", len = 1L, na.ok = FALSE)
  checkArg(format, choices = c("wide", "long"))
  checkArg(with.instance.id, "logical", len = 1L, na.ok = FALSE)

  jitter2 = ifelse(jitter, 0.05, 0)
  # for runtime tasks set to cutoff, otherwise use default, which is min or max value of perfs
  base = if (desc$performance_type[[measure]] == "runtime" && !is.na(desc$algorithm_cutoff_time))
    desc$algorithm_cutoff_time
  else
    NULL
  # potentially set zero values to something small here
  algo.runs = imputeAlgoPerf(astask, measure = measure,
    base = base, range.scalar = 0.3, jitter = jitter2, impute.zero.vals = impute.zero.vals)

  # include fake repetition col, so we can convert to wide
  algo.runs = aggregateStochasticAlgoRuns(algo.runs, measure = measure, with.repetition = TRUE)

  checkLogarithm(check.log, algo.runs, measure)
  data = if (format == "wide")
    convertAlgoPerfToWideFormat(algo.runs, desc = desc)
  else
    algo.runs
  # remove fake repetition col
  data$repetition = NULL
  if (!with.instance.id)
    data = dropNamed(data, "instance_id")
  list(data = data, measure = measure)
}


