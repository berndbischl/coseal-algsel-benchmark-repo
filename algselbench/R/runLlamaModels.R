#' @export
runLlamaModels = function(astask, nfolds = 10L, stratify = TRUE, classifiers, regressors) {
  if (missing(classifiers)) {
    # FIXME: use 2-3 classfiers and 2-3 regr models
    classifiers = list(j48 = J48)
  }
  if (missing(regressors)) {
    # FIXME: use 2-3 classfiers and 2-3 regr models
    regressors = list(lm = LinearRegression)
  }
  learners = c(classifiers, regressors)
  llama.funs = c(
    replicate(length(classifiers), classify, simplify = FALSE),
    replicate(length(regressors), regression, simplify = FALSE)
  )
  learner.ns = names(learners)
  llama.task = convertToLlama(astask)
  cv = cvFolds(llama.task, nfolds = nfolds, stratify = stratify)
  res = makeDataFrame(nrow = length(learners), ncol = 2L, col.types = "numeric", 
    row.names = learner.ns, col.names = c("mcp", "best"))
  for (i in 1:length(learners)) {
    learner = learners[[i]]
    n = learner.ns[[i]]
    llama.fun = llama.funs[[i]]
    z = llama.fun(learner, data = cv)
    res[n, "best"] = mean(unlist(successes(cv, res$predictions)))
    res[n, "mcp"] = mean(unlist(misclassificationPenalties(cv, z$predictions)))
  }
  return(res)
}
