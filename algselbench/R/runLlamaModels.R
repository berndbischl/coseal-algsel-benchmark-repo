#FIXME document that we can also use one task
#FIXME: remove feature stesp as list

#' @title Creates a registry which can be used for running several Llama models on a cluster.
#'
#' @description
#' It is likely that you need to install some additional R packages for this from CRAN or extra
#' Weka learner. The latter can be one via e.g. \code{WPM("install-package", "XMeans")}.
#'
#' Feature costs are added for real prognostic models but not for baseline models.
#'
#' Machine learning models are run with their default settings.
#' For the clusterers we set the number of clusters (N) to the number of algorithms in the task.
#' Except for XMeans where we set the maximum number of clusters (H) in the same way, as
#' a parameter for the number of clusters does not exist.
#'
#' @param astasks [\code{list}]\cr
#'   List of algorithm selection tasks (\code{\link{ASTask}}).
#' @param feature.steps [\code{list} of \code{character}]\cr
#'   Named list of feature steps we want to use.
#'   Must be named with task ids.
#' @param baselines [\code{character}]\cr
#'   Vector of characters, defining the baseline models.
#'   Default is c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses").
#' @param classifiers [\code{character}]\cr
#'   Vector of characters, defining the classification models.
#'   Default is none.
#' @param regressors [\code{character}]\cr
#'   Vector of characters, defining the regression models.
#'   Default is none.
#' @param clusterers [\code{character}]\cr
#'   Vector of characters, defining the cluster models.
#'   Default is none.
#' @param pre [\code{function}]\cr
#'   A function (e.g. normalize) to preprocess the feature data.
#'   By default no preprocessing is done.
#' @return BatchExperiments registry.
#' @export
runLlamaModels = function(astasks, feature.steps.list, baselines,
 classifiers = character(0), regressors = character(0), clusterers = character(0), pre) {

  checkArg(astasks, c("list", "ASTask"))
  if (!missing(astasks) && inherits(astasks, "ASTask"))
    astasks = list(astasks)
  checkListElementClass(astasks, "ASTask")
  task.ids = sapply(astasks, function(x) x$desc$task_id)

  checkArg(feature.steps.list, "list")
  checkListElementClass(feature.steps.list, "character")
  stopifnot(setequal(names(feature.steps.list), task.ids))
  # sort in correct order
  feature.steps.list = feature.steps.list[task.ids]

  if (missing(pre)) {
    pre = function(x, y = NULL) {
      list(features = x)
    }
  }

  # models and defaults
  baselines.all = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")

  # FIXME: we need a better interface to discrminate between learners from Weka, llama and mlr
  classifiers.weka = c("meta/AdaBoostM1", "bayes/BayesNet", "lazy/IBk", "rules/OneR",
    "trees/RandomTree", "trees/J48", "rules/JRip")
  classifiers.mlr = c("classif.ctree", "classif.kknn", "classif.ksvm", "classif.naiveBayes",
    "classif.randomForest", "classif.rpart")
  classifiers.all = c(classifiers.weka, classifiers.mlr)

  clusterers.weka = c("XMeans", "EM", "FarthestFirst", "SimpleKMeans")
  clusterers.all = clusterers.weka

  # check model args
  if (missing(baselines))
    baselines = baselines.all
  else
    checkArg(baselines, subset = baselines.all)
  # FIXME: the checks are bad in general... which models can be support?
  checkArg(classifiers, subset = classifiers.all)
  checkArg(regressors, "character", na.ok = FALSE)
  checkArg(clusterers, "character", na.ok = FALSE)

  packs = c("RWeka", "llama", "methods", "mlr", "BatchExperiments")
  requirePackages(packs, why = "runLlamaModels")

  makeModelFun = function(name, n.algos) {
    force(n.algos)
    if (str_detect(name, "classif\\.|regr\\.")) {
      convertMlrLearnerToLlama(makeLearner(name))
    } else if (name %in% classifiers.weka) {
      make_Weka_classifier(paste("weka/classifiers", name, sep = "/"))
    } else if (name %in% clusterers.weka) {
      # we set nr of clusters to nr of algos, for XMEANs it is the max nr, as we split by BIC value
      if (name == "XMeans") {
        function(data) {
          XMeans(data, control = Weka_control(H = n.algos))
        }
      } else if (name == "EM") {
        function(data) {
          wekaEM = make_Weka_clusterer("weka/clusterers/EM",
            init = Weka_control(N = n.algos))
          wekaEM(data)
        }
      } else if (name == "FarthestFirst") {
        function(data) {
          wekaFF = make_Weka_clusterer("weka/clusterers/FarthestFirst",
            init = Weka_control(N = n.algos))
          wekaFF(data)
        }
      } else if (name == "SimpleKMeans") {
        function(data) {
          wekaSimpleKMeans = make_Weka_clusterer("weka/clusterers/SimpleKMeans",
            init = Weka_control(N = n.algos))
          wekaSimpleKMeans(data)
        }
      } else {
        make_Weka_clusterer(paste("weka/clusterers", name, sep = "/"))
      }
    } else {
      get(name)
    }
  }

  # baseline models use these, no feature costs
  llama.tasks = mapply(convertToLlama, astask = astasks, feature.steps = feature.steps.list,
    MoreArgs = list(add.feature.costs = FALSE), SIMPLIFY = FALSE)
  # real models use these, use feature costs
  llama.cvs = mapply(convertToLlamaCVFolds, astask = astasks, feature.steps = feature.steps.list,
    MoreArgs = list(add.feature.costs = TRUE), SIMPLIFY = FALSE)
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
        feature.steps = feature.steps.list[[desc$task_id]],
        timeout = timeout,
        llama.task = llama.tasks[[i]],
        llama.cv = llama.cvs[[i]],
        makeModelFun = makeModelFun,
        n.algos = length(getAlgorithmNames(astask)),
        makeRes = function(data, p, timeout) {
          list(
            succ = mean(successes(data, p)),
            par10 = mean(parscores(data, p, timeout = timeout)),
            mcp = mean(misclassificationPenalties(data, p))
          )
        }
      )
    )
  }

  algoBaseline = function(static, llama.fun, model) {
    fun = static$makeModelFun(model, n.algos = static$n.algos)
    p = fun(data = static$llama.task)
    static$makeRes(static$llama.task, p, static$timeout)
  }

  algoLlama = function(static, llama.fun, model) {
    # FIXME: how to better load this? think about interactive test and so on...
    if (!interactive())
      library(algselbench)
    #FIXME: get from llama package envir
    llama.fun = get(llama.fun)
    fun = static$makeModelFun(model, n.algos = static$n.algos)
    p = llama.fun(fun, data = static$llama.cv, pre = pre)
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
