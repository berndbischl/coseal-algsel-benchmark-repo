checkMeasure = function(measure, desc) {
  if (missing(measure))
    measure = desc$performance_measures[1]
  else
    checkArg(measure, choices = desc$performance_measures)
  return(measure)
}
