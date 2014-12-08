convertToCheck = function(asscenario, measure, feature.steps, add.feature.costs) {
  assertClass(asscenario, "ASScenario")
  if (missing(measure))
    measure = asscenario$desc$performance_measures[1]
  else
    assertString(measure)
  allsteps = names(asscenario$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = getDefaultFeatureStepNames(asscenario)
  else
    assertSubset(feature.steps, allsteps)
  assertFlag(add.feature.costs)
  if (add.feature.costs && is.null(asscenario$feature.costs))
    warningf("Requested to add feature costs, but none in scenario. Adding always 0 feature costs.")
  return(list(measure = measure, feature.steps = feature.steps))
}
