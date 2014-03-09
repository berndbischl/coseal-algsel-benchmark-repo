#' Creates a registry which can be used for running several Llama models on a cluster.
#'
#' @param astasks [\code{list}]\cr
#'   List of algorithm selection tasks (\code{\link{ASTask}}).
#' @param nfolds [\code{integer(1)}]\cr
#'   Number of folds used for the crossvalidation of the data sets.
#'   Default is 10.
#' @param stratify [\code{logical(1)}]\cr
#'   Should the training and test data be stratified?
#'   Default is TRUE.
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
runLlamaModels = function(astasks, nfolds = 10L, stratify = TRUE, baselines, 
  classifiers, regressors, clusterers) {

  checkArg(astasks, c("list", "ASTask"))
  if (!missing(astasks) && inherits(astasks, "ASTask"))
    astasks = list(astasks)
  checkListElementClass(astasks, "ASTask")
  if (missing(baselines)) {
    baselines = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")
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
    requirePackages("party", "runLlamaModels")
    requirePackages("FNN", "runLlamaModels")
    requirePackages("kknn", "runLlamaModels")
    requirePackages("kernlab", "runLlamaModels")
    requirePackages("class", "runLlamaModels")
    requirePackages("nnet", "runLlamaModels")
    requirePackages("e1071", "runLlamaModels")
    requirePackages("randomForest", "runLlamaModels")
    requirePackages("klaR", "runLlamaModels")
    requirePackages("rpart", "runLlamaModels")
    classif.AdaBoost = make_Weka_classifier("weka/classifiers/meta/AdaBoostM1")
    classif.BayesNet = make_Weka_classifier("weka/classifiers/bayes/BayesNet")
    classif.IBk = make_Weka_classifier("weka/classifiers/lazy/IBk")
    classif.J48 = J48
    classif.JRip = JRip
    classif.OneR = make_Weka_classifier("weka/classifiers/rules/OneR")
    classif.MultilayerPerceptron = make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
    classif.RandomTree = make_Weka_classifier("weka/classifiers/trees/RandomTree")
    classif.boosting = convertMlrClassifLearnerToLlama(makeLearner("classif.boosting"))
    classif.ctree = convertMlrClassifLearnerToLlama(makeLearner("classif.ctree"))
    classif.fnn = convertMlrClassifLearnerToLlama(makeLearner("classif.fnn"))
    classif.kknn = convertMlrClassifLearnerToLlama(makeLearner("classif.kknn"))
    classif.ksvm = convertMlrClassifLearnerToLlama(makeLearner("classif.ksvm"))
    classif.naiveBayes = convertMlrClassifLearnerToLlama(makeLearner("classif.naiveBayes"))
    classif.nnet = convertMlrClassifLearnerToLlama(makeLearner("classif.nnet", size = 3L))
    classif.randomForest = convertMlrClassifLearnerToLlama(makeLearner("classif.randomForest"))
    classif.rpart = convertMlrClassifLearnerToLlama(makeLearner("classif.rpart"))
    classif.svm = convertMlrClassifLearnerToLlama(makeLearner("classif.svm")) 
  }
  if (missing(regressors)) {
    regressors = c("REPTree", "earth", "ksvm", "lm", "nnet", "randomForest", "rpart")
    regressors = paste("regr", regressors, sep = ".")
    requirePackages("earth", "runLlamaModels")
    requirePackages("kernlab", "runLlamaModels")
    requirePackages("stats", "runLlamaModels")
    requirePackages("nnet", "runLlamaModels")
    requirePackages("randomForest", "runLlamaModels")
    requirePackages("rpart", "runLlamaModels")  
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
  # FIXME: maybe parallel?
  llama.tasks = lapply(astasks, convertToLlama)
  ## remove constant columns
  llama.task = lapply(llama.tasks, removeConstantFeatures)
  ## generate cv-folds
  llama.cvs = lapply(llama.tasks, cvFolds, nfolds = nfolds, stratify = stratify)
  ## avoid empty classes
  llama.cvs = lapply(llama.cvs, avoidEmptyClasses)

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
            succ = mean(unlist(successes(data, p))),
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
    p = llama::classify(classifier = fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p)
  }

  algoRegr = function(static, model) {
    fun = get(model)
    p = llama::regression(regressor = fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p)
  }
  
  algoCluster = function(static, model) {
    fun = get(model)
    p = llama::cluster(clusterer = fun, data = static$llama.cv)$predictions
    static$makeRes(static$llama.cv, p)    
  }

  addAlgorithm(reg, id = "baseline", fun = algoBaseline)
  addAlgorithm(reg, id = "classif", fun = algoClassif)
  addAlgorithm(reg, id = "regr", fun = algoRegr)
  addAlgorithm(reg, id = "cluster", fun = algoCluster)
  
  des.baseline = makeDesign("baseline", exhaustive = list(model = baselines))
  des.classif = makeDesign("classif", exhaustive = list(model = classifiers))
  des.regr = makeDesign("regr", exhaustive = list(model = regressors))
  des.cluster = makeDesign("cluster", exhaustive = list(model = clusterers))
  addExperiments(reg, algo.designs = list(des.baseline, des.classif, des.regr, des.cluster))
  return(reg)
}
