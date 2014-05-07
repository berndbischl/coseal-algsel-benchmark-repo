convertToCheck = function(asscenario, measure, feature.steps, add.feature.costs) {
  checkArg(asscenario, "ASScenario")
  if (missing(measure))
    measure = asscenario$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  allsteps = names(asscenario$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = getDefaultFeatureStepNames(asscenario)
  else
    checkArg(feature.steps, subset = allsteps)
  checkArg(add.feature.costs, "logical", len = 1L, na.ok = FALSE)
  if (add.feature.costs && is.null(asscenario$feature.costs))
    warningf("Requested to add feature costs, but none in scenario. Adding always 0 feature costs.")
  return(list(measure = measure, feature.steps = feature.steps))
}
