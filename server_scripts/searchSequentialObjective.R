searchSequentialObjectiveFeatures = function(xs, ldf, llama.model.fun, mlr.learner) {
  scores = parallelMap(function(x) {
    sel = (x == 1)
    if (!any(sel))
      return(Inf)
    ldf2 = ldf
    ldf2$features = ldf$features[sel]
    #print(paste("selected:", paste(ldf2$features, collapse=", ")))
    model = llama.model.fun(mlr.learner, ldf2)
    score = mean(parscores(ldf2, model))
    return(score)
  }, xs, simplify = TRUE, level = "searchSequentialObjective")
  return(scores)
}

searchSequentialObjectiveSolvers = function(xs, ldf, llama.model.fun, mlr.learner) {
  scores = parallelMap(function(x) {
    sel = (x == 1)
    if (!any(sel))
      return(Inf)
    ldf2 = ldf
    ldf2$performance = ldf$performance[sel]
    #print(paste("selected:", paste(ldf2$performance, collapse=", ")))
    model = llama.model.fun(mlr.learner, ldf2)
    score = mean(parscores(ldf2, model))
    return(score)
  }, xs, simplify = TRUE, level = "searchSequentialObjective")
  return(scores)
}
