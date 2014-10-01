searchSequentialObjective = function(xs, ldf, llama.model.fun, mlr.learner) {
  scores = parallelMap(function(x) {
    print("eval")
    sel = (x == 1)
    if (!any(sel))
      return(Inf)
    ldf2 = ldf
    ldf2$features = ldf$features[sel]
    model = llama.model.fun(mlr.learner, ldf2)
    score = mean(parscores(ldf2, model))
    return(score)
  }, xs, simplify = TRUE, level = "searchSequentialObjective")
  return(scores)
}
