library(llama)
library(RWeka)
library(mlr)

load_all()

# ast = parseASScenario("/home/bischl/cos/coseal/data/sat12-rand")
# ast = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011")

bls = c("singleBest")
bls = character(0L)

learners = list(
  makeLearner("classif.rpart")
)

par.sets = list(
  classif.rpart = makeParamSet(makeIntegerParam("minsplit", lower = 1, upper = 10))
)

reg = runLlamaModels(ast, baselines = bls, learners = learners, par.sets = par.sets,
  rs.iters = 2)

# testJob(reg, 1, external = F)
submitJobs(reg)
d = reduceResultsExperiments(reg)
print(d)
