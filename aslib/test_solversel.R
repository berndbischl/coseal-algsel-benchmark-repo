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
#ast = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011/")
ast = parseASScenario("/home/larsko/work/coseal/data/QBF-2011/")
#
ldf = convertToLlamaCVFolds(ast)

n.bits = length(getAlgorithmNames(ast))
ctrl = makeSSControl(method = "sfs")
z = searchSequential(searchSequentialObjectiveSolvers, n.bits, control = ctrl, ldf = ldf,
  llama.model.fun = regression, mlr.learner = makeLearner("regr.lm"))




