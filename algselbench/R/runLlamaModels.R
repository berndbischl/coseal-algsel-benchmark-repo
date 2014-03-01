#' @export
runLlamaModels = function(astasks, nfolds = 10L, stratify = TRUE, baselines, classifiers, regressors,
  show.info = TRUE) {

  checkArg(astasks, c("list", "ASTask"))
  if (!missing(astasks) && inherits(astasks, "ASTask"))
    astasks = list(astasks)
  checkListElementClass(astasks, "ASTask")
  if (missing(baselines)) {
    baselines = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")
  }
  if (missing(classifiers)) {
    # FIXME: use 2-3 classfiers and 2-3 regr models
    classifiers = c("J48")
  }
  if (missing(regressors)) {
    # FIXME: use 2-3 classfiers and 2-3 regr models
    regressors = c("LinearRegression")
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
