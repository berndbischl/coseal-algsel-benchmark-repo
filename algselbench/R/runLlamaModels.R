#' @export
runLlamaModels = function(astask, nfolds = 10L, stratify = TRUE, classifiers, regressors,
  show.info = TRUE) {

  if (missing(classifiers)) {
    # FIXME: use 2-3 classfiers and 2-3 regr models
    classifiers = list(j48 = J48)
  }
  if (missing(regressors)) {
    # FIXME: use 2-3 classfiers and 2-3 regr models
    regressors = list(lm = LinearRegression)
  }
  baselines = list(vbs = vbs, single_best = singleBest, single_best_by_par = singleBestByPar,
    single_best_by_success = singleBestBySuccesses)
  llama.task = convertToLlama(astask)
  llama.cv = cvFolds(llama.task, nfolds = nfolds, stratify = stratify)

  job = function(name, obj, type, llama.task, llama.cv, show.info) {
    if (show.info)
      messagef("Computing: %s", name)
    if (type == "baseline") {
      p = obj(llama.task)
      data = llama.task
    } else {
      if (type == "classif")
        p = classify(obj, data = llama.cv)$predictions
      else
        p = regression(obj, data = llama.cv)$predictions
      data = llama.cv
    }
    c(
      mean(unlist(successes(data, p))),
      mean(unlist(misclassificationPenalties(data, p))),
      mean(unlist(parscores(data, p)))
    )
  }
  objs = c(baselines, classifiers, regressors)
  types = rep(c("baseline", "classif", "regr"),
    c(length(baselines), length(classifiers), length(regressors)))
  rows  = parallelMap(job, names(objs), objs, types,
    more.args = list(llama.task = llama.task, llama.cv = llama.cv, show.info = show.info))
  res = as.data.frame(do.call(rbind, rows))
  rownames(res) = names(objs)
  colnames(res) = c("best", "mcp", "par")
  return(res)
}
