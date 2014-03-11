getProvidedFeatures = function(astask, steps) {
  checkArg(astask, "ASTask")
  checkArg(steps, subset = names(astask$desc$feature_steps))
  allfeats = getFeatureNames(astask)
  step.list = astask$desc$feature_steps
  allsteps = names(step.list)
  notsteps = setdiff(allsteps, steps)
  print(step.list[notsteps])
  notfeatures = Reduce(union, step.list[notsteps])
  print(notfeatures)
}

# astask = parseASTask(task.dir)
# getProvidedFeatures(astask, "lobjois_featuretime")

