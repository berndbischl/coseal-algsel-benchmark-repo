#' @title Creates a registry which can be used for running several Llama models on a cluster.
#'
#' @description
#' It is likely that you need to install some additional R packages for this from CRAN or extra
#' Weka learner. The latter can be one via e.g. \code{WPM("install-package", "XMeans")}.
#'
#' Feature costs are added for real prognostic models but not for baseline models.
#'
#' @param astasks [\code{list}]\cr
#'   List of algorithm selection tasks (\code{\link{ASTask}}).
#' @param baselines [\code{character}]\cr
#'   Vector of characters, defining the baseline models.
#'   Default is c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses").
#' @param classifiers [\code{character}]\cr
#'   Vector of characters, defining the classification models.
#'   Default is c("AdaBoost", "BayesNet", "IBk", "OneR", "MultilayerPerceptron",
#'   "RandomTree", "ctree", "fnn", "J48", "JRip", "kknn", "ksvm", "naiveBayes",
#'   "nnet", "randomForest", "rpart", "svm").
#' @param regressors [\code{character}]\cr
#'   Vector of characters, defining the regression models.
#'   Default is c("REPTree", "earth", "ksvm", "lm", "nnet", "randomForest", "rpart").
#' @param clusterers [\code{character}]\cr
#'   Vector of characters, defining the cluster models.
#'   Default is c("XMeans", "EM", "FarthestFirst", "SimpleKMeans").
#' @param pre [\code{function}]\cr
#'   A function (e.g. normalize) to preprocess the data.
#'   By default no preprocessing is done.
#' @return BatchExperiments registry.
#' @export
runLlamaModels = function(astasks, baselines, classifiers, regressors, clusterers, pre) {

  checkArg(astasks, c("list", "ASTask"))
  if (!missing(astasks) && inherits(astasks, "ASTask"))
    astasks = list(astasks)
  checkListElementClass(astasks, "ASTask")

  if (missing(pre)) {
    pre = function(x, y = NULL) {
      list(features = x)
    }    
  }
  
  # models and defaults
  baselines.def = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")

  classifiers.weka = c("meta/AdaBoostM1", "bayes/BayesNet", "lazy/IBk", "rules/OneR",
    "trees/RandomTree", "trees/J48", "rules/JRip")
  classifiers.mlr = c("classif.ctree", "classif.kknn", "classif.ksvm", "classif.naiveBayes",
    "classif.randomForest", "classif.rpart")
  classifiers.def = c(classifiers.weka, classifiers.mlr)

  regressors.mlr = c("regr.earth", "regr.lm", "regr.randomForest", "regr.rpart")
  regressors.def = regressors.mlr

  clusterers.weka = c("XMeans", "EM", "FarthestFirst", "SimpleKMeans")
  clusterers.def = clusterers.weka

  # check model args
  if (missing(baselines))
    baselines = baselines.def
  else
    checkArg(baselines, subset = baselines.def)
  if (missing(classifiers))
    classifiers = classifiers.def
  else
    checkArg(classifiers, subset = classifiers.def)
  if (missing(regressors))
    regressors = regressors.def
  else
    checkArg(regressors, subset = regressors.def)
  if (missing(clusterers))
    clusterers = clusterers.def
  else
    checkArg(clusterers, subset = clusterers.def)

  packs = c("RWeka", "mlr", "BatchExperiments")
  requirePackages(packs, why = "runLlamaModels")

  makeModelFun = function(name) {
    if (name %in% c(classifiers.mlr, regressors.mlr)) {
      convertMlrLearnerToLlama(makeLearner(name))
    } else if (name %in% classifiers.weka) {
      make_Weka_classifier(paste("weka/classifiers", name, sep = "/"))
    } else if (name %in% clusterers.weka) {
      make_Weka_clusterer(paste("weka/clusterers", name, sep = "/"))
    } else {
      get(name)
    }
  }

  # baseline models use these, no feature costs
  llama.tasks = lapply(astasks, convertToLlama, add.feature.costs = FALSE)
  # real models use these, use feature costs
  llama.cvs = lapply(astasks, convertToLlamaCVFolds, add.feature.costs = TRUE)

  # FIXME:
  unlink("run_llama_models-files", recursive = TRUE)
  reg = makeExperimentRegistry("run_llama_models", packages = packs)

  for (i in seq_along(astasks)) {
    astask = astasks[[i]]
    desc = astask$desc
    cutoff = desc$algorithm_cutoff_time
    timeout = if (desc$performance_type[[1L]] == "runtime" && !is.na(cutoff))
      cutoff
    else
      NULL
    addProblem(reg, id = desc$task_id,
      static = list(
        timeout = timeout,
        llama.task = llama.tasks[[i]],
        llama.cv = llama.cvs[[i]],
        makeModelFun = makeModelFun,
        makeRes = function(data, p, timeout) {
          list(
            succ = mean(unlist(successes(data, p))),
            par10 = mean(unlist(parscores(data, p, timeout = timeout))),
            mcp = mean(unlist(misclassificationPenalties(data, p)))
          )
        }
      )
    )
  }

  algoBaseline = function(static, llama.fun, model) {
    fun = static$makeModelFun(model)
    p = fun(data = static$llama.task)
    static$makeRes(static$llama.task, p, static$timeout)
  }

  algoLlama = function(static, llama.fun, model) {
    # FIXME: how to better load this? think about interactive test and so on...
    if (!interactive())
      library(algselbench)
    #FIXME: get from llama package envir
    llama.fun = get(llama.fun)
    fun = static$makeModelFun(model)
    p = llama.fun(fun, data = static$llama.cv, pre = pre)$predictions
    static$makeRes(static$llama.cv, p, static$timeout)
  }

  addExps = function(id, algo, llama.fun, models) {
    if (length(models) > 0L) {
      addAlgorithm(reg, id = id, fun = algo)
      des = makeDesign(id, exhaustive = list(model = models, llama.fun = llama.fun))
      addExperiments(reg, algo.designs = des)
    }
  }

  addExps("baseline", algoBaseline, "foo", baselines)
  addExps("classif", algoLlama, "classify", classifiers)
  addExps("regr", algoLlama, "regression", regressors)
  addExps("cluster", algoLlama, "cluster", clusterers)

  return(reg)
}
