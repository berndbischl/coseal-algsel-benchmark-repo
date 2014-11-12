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
#' @param asscenarios [(list of) \code{\link{ASScenario}}]\cr
#'   Algorithm selection scenarios.
#' @param feature.steps [\code{list} of \code{character}]\cr
#'   Named list of feature steps we want to use.
#'   Must be named with scenario ids.
#'   Default is to take the default feature steps from the scenario.
#' @param baselines [\code{character}]\cr
#'   Vector of characters, defining the baseline models.
#'   Default is c("vbs", "singleBest", "singleBestByPar", "singleBestBySuccesses").
#' @param classifiers [(list of) \code{\link[mlr]{Learner}}]\cr
#'   Classification learners.
#'   Default is none.
#' @param regressors [(list of) \code{\link[mlr]{Learner}}]\cr
#'   Regression learners.
#'   Default is none.
#' @param clusterers [(list of) \code{\link[mlr]{Learner}}]\cr
#'   Cluster learners.
#'   Default is none.
#' @param pre [\code{function}]\cr
#'   A function (e.g. normalize) to preprocess the feature data.
#'   By default no preprocessing is done.
#' @return BatchExperiments registry.
#' @export
runLlamaModels = function(asscenarios, feature.steps.list = NULL, baselines,
 classifiers = list(), regressors = list(), clusterers = list(), pre) {

  asscenarios = ensureVector(asscenarios, 1L, cl = "ASScenario")
  assertList(asscenarios, types = "ASScenario")
  scenario.ids = extractSubList(asscenarios, c("desc", "scenario_id"), use.names = FALSE)
  names(asscenarios) = scenario.ids

  if (is.null(feature.steps.list)) {
    feature.steps.list = extractSubList(asscenarios, c("desc", "default_steps"),
      simplify = FALSE, use.names = TRUE)
  } else {
    feature.steps.list = ensureVector(feature.steps.list, 1L, cl = "character")
    assertList(feature.steps.list, types = "character", names = "unique")
    assertSetEqual(names(feature.steps.list), scenario.ids, ordered = FALSE)
  }
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

  llama.scenarios = mapply(convertToLlama, asscenario = asscenarios, feature.steps = feature.steps.list,
    SIMPLIFY = FALSE)
  llama.cvs = lapply(asscenarios, convertToLlamaCVFolds)

  # FIXME:
  unlink("run_llama_models-files", recursive = TRUE)
  reg = makeExperimentRegistry("run_llama_models", packages = packs)

  for (i in seq_along(asscenarios)) {
    asscenario = asscenarios[[i]]
    desc = asscenario$desc
    cutoff = desc$algorithm_cutoff_time
    timeout = if (desc$performance_type[[1L]] == "runtime" && !is.na(cutoff)) {
      cutoff
    } else {
      NULL
    }
    addProblem(reg, id = desc$scenario_id,
      static = list(
        feature.steps = feature.steps.list[[desc$scenario_id]],
        timeout = timeout,
        llama.scenario = llama.scenarios[[i]],
        llama.cv = llama.cvs[[i]],
        n.algos = length(getAlgorithmNames(asscenario)),
        makeRes = function(data, p, timeout, addCosts) {
          if(addCosts) {
            data = fixFeckingPresolve(asscenario, data)
          }
          list(
            succ = mean(successes(data, p, timeout = timeout, addCosts = addCosts)),
            par10 = mean(parscores(data, p, timeout = timeout, addCosts = addCosts)),
            mcp = mean(misclassificationPenalties(data, p))
          )
        }
      )
    )
  }

  # add baselines to reg
  addAlgorithm(reg, id = "baseline", fun = function(static, type) {
    llama.fun = get(type, envir = asNamespace("llama"))
    p = llama.fun(data = static$llama.scenario)
    p = list(predictions = p)
    # this is how LLAMA checks what type of argument is given to the evaluation function
    attr(p, "hasPredictions") = TRUE
    static$makeRes(static$llama.scenario, p, static$timeout, FALSE)
  })
  des = makeDesign("baseline", exhaustive = list(type = baselines))
  addExperiments(reg, algo.designs = des)

  # add real selectors
  addLearnerAlgoAndExps = function(lrn) {
    # BE does not like the dots in mlr ids
    id = str_replace_all(lrn$id, "\\.", "_")
    addAlgorithm(reg, id = id, fun = function(static) {
      llama.fun = switch(lrn$type,
        classif = llama::classify,
        regr = llama::regression,
        cluster = llama::cluster
      )
      p = llama.fun(lrn, data = static$llama.cv, pre = pre)
      static$makeRes(static$llama.cv, p, static$timeout, TRUE)
    })
    addExperiments(reg, algo.designs = id)
  }

  lapply(c(classifiers, regressors, clusterers), addLearnerAlgoAndExps)

  return(reg)
}
