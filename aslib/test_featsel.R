library(llama)
library(parallelMap)
library(mlr)
library(checkmate)
library(devtools)
# load_all("~/cos/mlr")
load_all(".")

#source("~/cos/coseal-algsel-benchmark-repo/server_scripts/searchSequential.R")
#source("~/cos/coseal-algsel-benchmark-repo/server_scripts/searchSequentialObjective.R")
source("~/work/coseal-algsel-benchmark-repo/server_scripts/searchSequential.R")
source("~/work/coseal-algsel-benchmark-repo/server_scripts/searchSequentialObjective.R")

# ast = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011")
#ast = parseASScenario("/home/bischl/cos/coseal/data/CSP-2010/")
ast = parseASScenario("/home/larsko/work/coseal/data/CSP-2010/")
#
ldf = convertToLlamaCVFolds(ast)

n.bits = length(getFeatureNames(ast))
ctrl = makeSSControl(method = "sbs")
z = searchSequential(searchSequentialObjectiveFeatures, n.bits, control = ctrl, ldf = ldf,
  llama.model.fun = classify, mlr.learner = makeLearner("classif.rpart"))




