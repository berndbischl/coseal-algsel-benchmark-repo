#' @export
runLlamaModels = function(astasks, nfolds = 10L, stratify = TRUE, baselines, classifiers, regressors, packages,
  show.info = TRUE) {

  checkArg(astasks, c("list", "ASTask"))
  if (!missing(astasks) && inherits(astasks, "ASTask"))
    astasks = list(astasks)
  checkListElementClass(astasks, "ASTask")
  if (missing(baselines)) {
    ## FIXME: singleBestByPar and singleBestBySuccesses did not exist in Llama
    ## --> created a helper function, which can be found in the R-code of the package
  #    baselines = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")
    baselines = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccess")
  }
  ## FIXME: find a way to pass further arguments such as size in nnet
  ## FIXME: problems with rpart and lda (due to constant values), presumably caused
  ## by %dopar% in llama::classify
  ## FIXME: also in %dopar%, problem when training a model to an empty class, e.g.
  ## in SAT11-Crafted fold 1 (of 2 folds)
  if (missing(classifiers)) {
    classifiers = c("J48", "randomForest", "svm")
    requirePackages("e1071", "runLlamaModels")
    requirePackages("randomForest", "runLlamaModels")
  }
  if (missing(regressors)) {
    regressors = c("LinearRegression", "lm", "randomForest")
    requirePackages("randomForest", "runLlamaModels")
    requirePackages("stats", "runLlamaModels")
  }
  if (!missing(packages)) {
    for(pack in packages) {
      requirePackages(pack, "runLlamaModels")
    }
  }
  # FIXME: maybe parallel?
  llama.tasks = lapply(astasks, convertToLlama)
  llama.cvs = lapply(llama.tasks, cvFolds, nfolds = nfolds, stratify = stratify)

  #FIXME: do we really need mlr just for impute
  requirePackages("BatchExperiments", "runLlamaModels")
  # FIXME:
  unlink("run_llama_models-files", recursive = TRUE)
  reg = makeExperimentRegistry("run_llama_models", packages = c("llama", "RWeka"))

  for (i in seq_along(astasks)) {
    addProblem(reg, id = astasks[[i]]$desc$task_id,
      static = list(
        llama.task = llama.tasks[[i]],
        llama.cv = llama.cvs[[i]],
        makeRes = function(data, p) {
          list(
            best = mean(unlist(successes(data, p))),
            mcp = mean(unlist(misclassificationPenalties(data, p))),
            par = mean(unlist(parscores(data, p)))
          )
        }
      )
    )
  }

  algoBaseline = function(static, model) {
    fun = get(model)
    p = fun(static$llama.task)
    static$makeRes(static$llama.task, p)
  }

  algoClassif = function(static, model) {
    fun = get(model)
    p = classify(fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p)
  }

  algoRegr = function(static, model) {
    fun = get(model)
    p = regression(fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p)
  }

  addAlgorithm(reg, id = "baseline", fun = algoBaseline)
  addAlgorithm(reg, id = "classif", fun = algoClassif)
  addAlgorithm(reg, id = "regr", fun = algoRegr)

  des.baseline = makeDesign("baseline", exhaustive = list(model = baselines))
  des.classif = makeDesign("classif", exhaustive = list(model = classifiers))
  des.regr = makeDesign("regr", exhaustive = list(model = regressors))
  addExperiments(reg, algo.designs = list(des.baseline, des.classif, des.regr))
  return(reg)
}
