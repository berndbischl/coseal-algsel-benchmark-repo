removeCensoredRuns = function(asscenario) {
  ## set runtime of non-ok runs to NA
  performances = lapply(asscenario$desc$performance_measures, function(measure) {
    ifelse(asscenario$algo.runs$runstatus == "ok", asscenario$algo.runs[[measure]], NA_real_)
  })
  names(performances) = asscenario$desc$performance_measures
  asscenario$algo.runs[, asscenario$desc$performance_measures] = drop(do.call(cbind, performances))
  return(asscenario)
}
