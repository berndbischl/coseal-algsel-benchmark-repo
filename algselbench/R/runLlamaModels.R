#FIXME: the whole way of defining/selecting the learner is horrible in the code!

#' Creates a registry which can be used for running several Llama models on a cluster.
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
#' @return registry.
#' @export
runLlamaModels = function(astasks, baselines, classifiers, regressors, clusterers) {

  checkArg(astasks, c("list", "ASTask"))
  if (!missing(astasks) && inherits(astasks, "ASTask"))
    astasks = list(astasks)
  checkListElementClass(astasks, "ASTask")
  baselines.def = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")
  if (missing(baselines)) {
    baselines = baselines.def
  } else {
    checkArg(baselines, subset = baselines.def)
  }
  ## FIXME: find a way to pass further arguments such as size in nnet
  ## FIXME: problems with rpart and lda (due to constant values), presumably caused
  ## by %dopar% in llama::classify
  ## FIXME: also in %dopar%, problem when training a model to an empty class, e.g.
  ## in SAT11-Crafted fold 1 (of 2 folds)
  if (missing(classifiers)) {
    classifiers = c("AdaBoost", "BayesNet", "IBk", "OneR", "MultilayerPerceptron",
      "RandomTree", "ctree", "fnn", "J48", "JRip", "kknn", "ksvm",
      "naiveBayes", "nnet", "randomForest", "rpart", "svm")
    classifiers = paste("classif", classifiers, sep = ".")
    requirePackages(why="runLlamaModels", packs = c("party", "FNN", "kknn", "kernlab",
        "class", "nnet", "e1071", "randomForest", "klaR", "rpart"))
    classif.AdaBoost = make_Weka_classifier("weka/classifiers/meta/AdaBoostM1")
    classif.BayesNet = make_Weka_classifier("weka/classifiers/bayes/BayesNet")
    classif.IBk = make_Weka_classifier("weka/classifiers/lazy/IBk")
    classif.J48 = J48
    classif.JRip = JRip
    classif.OneR = make_Weka_classifier("weka/classifiers/rules/OneR")
    classif.MultilayerPerceptron = make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
    classif.RandomTree = make_Weka_classifier("weka/classifiers/trees/RandomTree")
    g = function(x, ...)
      convertMlrClassifLearnerToLlama(makeLearner(x, ...))
    classif.boosting = g("classif.boosting")
    classif.ctree = g("classif.ctree")
    classif.fnn = g("classif.fnn")
    classif.kknn = g("classif.kknn")
    classif.ksvm = g("classif.ksvm")
    classif.naiveBayes = g("classif.naiveBayes")
    classif.nnet = g("classif.nnet", size = 3L)
    classif.randomForest = g("classif.randomForest")
    classif.rpart = g("classif.rpart")
    classif.svm = g("classif.svm")
  }
  if (missing(regressors)) {
    regressors = c("REPTree", "earth", "ksvm", "lm", "nnet", "randomForest", "rpart")
    regressors = paste("regr", regressors, sep = ".")
    requirePackages(why = "runLlamaModels", packs = c("earth", "kernlab", "stats", "nnet",
      "randomForest", "rpart"))
    regr.REPTree = make_Weka_classifier("weka/classifiers/trees/REPTree")
    regr.earth = earth
    regr.ksvm = ksvm
    regr.lm = lm
    regr.nnet = function(formula, data, ...) nnet(formula = formula, data = data, size = 3L, ...)
    regr.randomForest = randomForest
    regr.rpart = rpart
  }
  if (missing(clusterers)) {
    clusterers = c("XMeans", "EM", "FarthestFirst", "SimpleKMeans")
    clusterers = paste("cluster", clusterers, sep = ".")
    cluster.XMeans = XMeans
    cluster.EM = make_Weka_clusterer("weka/clusterers/EM")
    cluster.FarthestFirst = make_Weka_clusterer("weka/clusterers/FarthestFirst")
    cluster.SimpleKMeans = make_Weka_clusterer("weka/clusterers/SimpleKMeans")
  }


  #FIXME: baseline methods are not cross-validated.

  llama.tasks = lapply(astasks, convertToLlama)
  llama.cvs = lapply(astasks, convertToLlamaCVFolds)

  requirePackages("BatchExperiments", why = "runLlamaModels")
  # FIXME:
  unlink("run_llama_models-files", recursive = TRUE)
  reg = makeExperimentRegistry("run_llama_models", packages = c("llama", "RWeka"))

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

  algoBaseline = function(static, model) {
    fun = get(model)
    p = fun(static$llama.task)
    static$makeRes(static$llama.task, p, static$timeout)
  }

  algoClassif = function(static, model) {
    fun = get(model)
    p = llama::classify(classifier = fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p, static$timeout)
  }

  algoRegr = function(static, model) {
    fun = get(model)
    p = llama::regression(regressor = fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p, static$timeout)
  }

  algoCluster = function(static, model) {
    fun = get(model)
    p = llama::cluster(clusterer = fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p, static$timeout)
  }

  addExps = function(id, models, fun) {
    if (length(models) > 0L) {
      addAlgorithm(reg, id = id, fun = fun)
      des = makeDesign(id, exhaustive = list(model = models))
      addExperiments(reg, algo.designs = des)
    }
  }

  addExps("baseline", baselines, algoBaseline)
  addExps("classif", classifiers, algoClassif)
  addExps("regr", regressors, algoRegr)
  addExps("cluster", clusterers, algoCluster)

  return(reg)
}
