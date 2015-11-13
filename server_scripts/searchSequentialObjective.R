searchSequentialObjectiveFeatures = function(xs, scenario, ldf, llama.model.fun, mlr.learner) {
  scores = parallelMap(function(x) {
    sel = (x == 1)
    if (!any(sel))
      return(Inf)
    ldf2 = ldf
    ldf2$features = ldf$features[sel]
    # print(ldf2$features)
    #print(paste("selected:", paste(ldf2$features, collapse=", ")))
    model = llama.model.fun(mlr.learner, ldf2)
    ldf3 = fixFeckingPresolve(scenario, ldf2)
    score = mean(parscores(ldf3, model))
    return(score)
  }, xs, simplify = TRUE)
  return(scores)
}

searchSequentialObjectiveSolvers = function(xs, scenario, ldf, llama.model.fun, mlr.learner) {
  scores = parallelMap(function(x) {
    sel = (x == 1)
    if (!any(sel))
      return(Inf)
    ldf2 = ldf
    ldf2$performance = ldf$performance[sel]
    ldf2$success = ldf$success[sel]
    model = llama.model.fun(mlr.learner, ldf2)
    ldf3 = fixFeckingPresolve(scenario, ldf2)
    score = mean(parscores(ldf3, model))
    return(score)
  }, xs, simplify = TRUE)
  return(scores)
}
