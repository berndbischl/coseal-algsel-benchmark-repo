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
#' For the clusterers we set the number of clusters (N) to the number of algorithms in the scenario.
#' Except for XMeans where we set the maximum number of clusters (H) in the same way, as
#' a parameter for the number of clusters does not exist.
#'
#' @param asscenarios [\code{list}]\cr
#'   List of algorithm selection scenarios (\code{\link{ASScenario}}).
#' @param feature.steps [\code{list} of \code{character}]\cr
#'   Named list of feature steps we want to use.
#'   Must be named with scenario ids.
#' @param baselines [\code{character}]\cr
#'   Vector of characters, defining the baseline models.
#'   Default is c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses").
#' @param classifiers [list of \code{\link[mlr]{Learner}}]\cr
#'   Classification learners.
#'   Default is none.
#' @param regressors [list of \code{\link[mlr]{Learner}}]\cr
#'   Regression learners.
#'   Default is none.
#' @param clusterers [list of \code{\link[mlr]{Learner}}]\cr
#'   Cluster learners.
#'   Default is none.
#' @param pre [\code{function}]\cr
#'   A function (e.g. normalize) to preprocess the feature data.
#'   By default no preprocessing is done.
#' @return BatchExperiments registry.
#' @export
runLlamaModels = function(asscenarios, feature.steps.list, baselines,
 classifiers = list(), regressors = list(), clusterers = list(), pre) {

  asscenarios = ensureVector(asscenarios, 1L, cl = "ASScenario")
  assertList(asscenarios, types = "ASScenario")
  scenario.ids = sapply(asscenarios, function(x) x$desc$scenario_id)

  assertList(feature.steps.list, types = "character")
  # FIXME remove bad check
  stopifnot(setequal(names(feature.steps.list), scenario.ids))
  # sort in correct order
  feature.steps.list = feature.steps.list[scenario.ids]

  if (missing(pre)) {
    pre = function(x, y = NULL) {
      list(features = x)
    }
  }

  # models and defaults
  baselines.all = c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses")

  # check model args
  if (missing(baselines))
    baselines = baselines.all
  else
    assertSubset(baselines, baselines.all)
  baselines = lapply(baselines, get, envir = asNamespace("llama"))

  checkLearners = function(lrns, type, arg.name) {
    lrns = ensureVector(lrns, n = 1L, cl = "Learner")
    assertList(lrns, types = "Learner")
    types = extractSubList(lrns, "type")
    if (any(types != type))
      stopf("%s: All learners must be of type '%s'!", arg.name, type)
    lrns
  }
  classifiers = checkLearners(classifiers, "classif", "classifiers")
  regressors = checkLearners(regressors, "regr", "regressors")
  clusterers = checkLearners(clusterers, "cluster", "clusterers")

  packs = c("RWeka", "llama", "methods", "mlr", "BatchExperiments")
  requirePackages(packs, why = "runLlamaModels")

  # baseline models use these, no feature costs
  llama.scenarios = mapply(convertToLlama, asscenario = asscenarios, feature.steps = feature.steps.list,
    MoreArgs = list(add.feature.costs = FALSE), SIMPLIFY = FALSE)
  # real models use these, use feature costs
  llama.cvs = mapply(convertToLlamaCVFolds, asscenario = asscenarios, feature.steps = feature.steps.list,
    MoreArgs = list(add.feature.costs = TRUE), SIMPLIFY = FALSE)
  llama.cvs = lapply(asscenarios, convertToLlamaCVFolds, add.feature.costs = TRUE)

  # FIXME:
  unlink("run_llama_models-files", recursive = TRUE)
  reg = makeExperimentRegistry("run_llama_models", packages = packs)

  for (i in seq_along(asscenarios)) {
    asscenario = asscenarios[[i]]
    desc = asscenario$desc
    cutoff = desc$algorithm_cutoff_time
    timeout = if (desc$performance_type[[1L]] == "runtime" && !is.na(cutoff))
      cutoff
    else
      NULL
    addProblem(reg, id = desc$scenario_id,
      static = list(
        feature.steps = feature.steps.list[[desc$scenario_id]],
        timeout = timeout,
        llama.scenario = llama.scenarios[[i]],
        llama.cv = llama.cvs[[i]],
        n.algos = length(getAlgorithmNames(asscenario)),
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
    p = model(data = static$llama.scenario)
    p = list(predictions = p)
    static$makeRes(static$llama.scenario, p, static$timeout)
  }

  algoLlama = function(static, llama.fun, model) {
    llama.fun = get(llama.fun, envir = asNamespace("llama"))
    p = llama.fun(model, data = static$llama.cv, pre = pre)
    static$makeRes(static$llama.cv, p, static$timeout)
  }

  addExps = function(id, algo, llama.fun, models) {
    if (length(models) > 0L) {
      addAlgorithm(reg, id = id, fun = algo)
      des = makeDesign(id, exhaustive = list(model = unlist(models), llama.fun = llama.fun))
      addExperiments(reg, algo.designs = des)
    }
  }

  addExps("baseline", algoBaseline, "foo", baselines)
  addExps("classif", algoLlama, "classify", classifiers)
  addExps("regr", algoLlama, "regression", regressors)
  addExps("cluster", algoLlama, "cluster", clusterers)

  return(reg)
}
