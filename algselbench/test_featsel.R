library(llama)
library(parallelMap)
# load_all("~/cos/mlr")
load_all(".")

source("/home/bischl/cos/coseal-algsel-benchmark-repo/server_scripts/searchSequential.R")
source("/home/bischl/cos/coseal-algsel-benchmark-repo/server_scripts/searchSequentialObjective.R")

# ast = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011")
ast = parseASScenario("/home/bischl/cos/coseal/data/PREMARSHALLING-ASTAR-2013/")
#
# ldf = convertToLlamaCVFolds(ast)

n.bits = length(getFeatureNames(ast))
ctrl = makeSSControl(method = "sbs")
z = searchSequential(searchSequentialObjective, n.bits, control = ctrl, ldf = ldf,
  llama.model.fun = classify, mlr.learner = makeLearner("classif.rpart"))




