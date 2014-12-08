# helper to convert perf to llama or mlr
convertPerf = function(asscenario, measure, feature.steps, add.feature.costs, with.instance.id) {
  desc = asscenario$desc
  # note that perf + runstatus + successes is ordered by instance, then repetition
  perf = convertAlgoPerfToWideFormat(desc, asscenario$algo.runs, measure)
  runstatus = asscenario$algo.runstatus
  cutoff = desc$algorithm_cutoff_time

  # FIXME: From here on the whole code does NOT work if we have repetitions
  # The reason is that we have not cleanly defined, what happens if on
  # one instance an algo crashes a sometimes but works otherwise 
  stopifnot(max(perf$repetition) == 1L)
  perf$repetition = runstatus$repetition = NULL
  iid = perf$instance_id
  perf$instance_id = runstatus$instance_id = NULL

  # construct successes, so far means: no NA in perf val and run status of algo is "OK"
  successes = !is.na(perf) & runstatus == "ok"
  # Note that all stuff in this object is ordered by instance_id
  presolve = getCostsAndPresolvedStatus(asscenario, feature.steps = feature.steps)

  # impute performance values and add feature costs for run time scenarios
  if (desc$performance_type[measure] == "runtime" & !is.na(cutoff)) {
    impute.val = desc$algorithm_cutoff_time
    if (add.feature.costs) {
      m = ncol(perf)
      # set algorithm costs to 0 for presolved instances, they wont run
      perf[presolve$is.presolved, ] = 0
      if (is.null(asscenario$feature.costs))
        add = 0
      else
        add = matrix(rep(presolve$costs, m), ncol = m, byrow = FALSE)
      # add instance costs (adapated by presolving) to each alg column
      perf = perf + add
    }
    # recalculate successes wrt to new perf vals and cutoff. we spent more time due to feat costs
    successes = successes & perf <= cutoff
  } else {
    impute.val = 10 * max(perf, na.rm = TRUE)
  }
  perf[!successes] = impute.val
  
  # FIXME: see above
  # aggregate stochastic algorithms, only do this if repeated measurements to save time
  # if (max(perf$repetition) > 1L) {
    # perf = ddply(perf, c("instance_id", "algorithm"), function(d) {
      # colMeans(d[, measure, drop = FALSE])
    # })
  # } else {
    # perf$repetition = NULL
  # }
  
  if (with.instance.id) {
    perf = cbind(instance_id = iid, perf)
    successes = cbind(instance_id = iid, as.data.frame(successes))
  }
  list(perf = perf, successes = successes)
}
