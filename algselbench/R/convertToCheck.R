convertToCheck = function(astask, measure, feature.steps, add.feature.costs) {
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  allsteps = names(astask$desc$feature_steps)
  if (missing(feature.steps))
    feature.steps = getDefaultFeatureStepNames(astask)
  else
    checkArg(feature.steps, subset = allsteps)
  checkArg(add.feature.costs, "logical", len = 1L, na.ok = FALSE)
  if (add.feature.costs && is.null(astask$feature.costs))
    warningf("Requested to add feature costs, but none in task. Adding always 0 feature costs.")
  return(list(measure = measure, feature.steps = feature.steps))
}
