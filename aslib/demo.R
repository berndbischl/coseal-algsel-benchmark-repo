load_all()
library(BatchExperiments)

scen = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011/")

print(scen)


getAlgorithmNames(scen)
getFeatureStepNames(scen)
getInstanceNames(scen)

summarizeAlgoRunstatus(scen)

plotAlgoCorMatrix(scen)

plotAlgoPerfBoxplots(scen)

findDominatedAlgos(scen)

llama.input = convertToLlama(scen)
# convertToMlr

classifiers = c("classif.OneR")
regressors = c("regr.lm")

scenarios = list(scen)
steps = list("QBF-2011" = "all_feats")

reg = runLlamaModels(scenarios, feature.steps.list = steps,
  classifiers = classifiers, regressors = regressors)
res = reduceResultsExperiments(reg)

