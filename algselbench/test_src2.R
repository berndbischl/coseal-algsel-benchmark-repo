library(llama)
library(RWeka)
load_all(".")


ast = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011")
reg = runLlamaModels(ast, baselines = c("vbs"),
  # classifiers = makeLearner("classif.rpart"),
  # regressors = makeLearner("regr.lm"),
  # clusterers = makeLearner("cluster.SimpleKMeans")
)
submitJobs(reg)
waitForJobs(reg)
d = reduceResultsExperiments(reg)
# z = testJob(reg, 1, external = T)

# lt = convertToLlamaCVFolds(ast, add.feature.costs = F)
# lres = classify(classifier = J48, data = lt)
# mcps1 = unlist(misclassificationPenalties(lt, lres$predictions))
# mcp1 = mean(mcps1)
# print(summary(mcps1))
# print(mcp1)

