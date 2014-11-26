library(BBmisc)
library(devtools)
library(llama)
library(stringr)
library(mlr)
library(ParamHelpers)
library(BatchExperiments)
load_all("../algselbench")
source("defs.R")

ds.dirs = list.files(file.path(coseal.svn.dir, "data"), full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "BBOB|MACHINE")][6]
print(ds.dirs)
# ds.dirs = ds.dirs[1]
asscenarios = lapply(ds.dirs, parseASScenario)

# untuned learners, so we can see whether tuning helped
learners = list(
  # classif
  makeLearner("classif.rpart"),
  makeLearner("classif.randomForest"),
  makeLearner("classif.ksvm"),
  # regr
  makeLearner("regr.lm"),
  makeLearner("regr.rpart"),
  makeLearner("regr.randomForest"),
  makeLearner("regr.mars"),
  # cluster
  makeLearner("cluster.XMeans", H = 30) # increase upper limit of clusters
)

par.sets = list(
  # classif
  classif.rpart = makeParamSet(),
  classif.randomForest = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 1000),
    makeIntegerParam("mtry", lower = 1, upper = 30)
  ),
  classif.ksvm = makeParamSet(
    makeNumericParam("C",     lower = -12, upper = 12, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
  ),
  # regr
  regr.lm = makeParamSet(),
  regr.rpart = makeParamSet(),
  regr.randomForest = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 1000),
    makeIntegerParam("mtry", lower = 1, upper = 30)
  ),
  regr.mars = makeParamSet(
    makeIntegerParam("degree", lower = 1, upper = 4),
    makeIntegerParam("penalty", lower = 0.5, upper = 5),
    makeLogicalParam("prune"),
    makeLogicalParam("forward.step")
  ),
  # cluster
  cluster.XMeans = makeParamSet()
)

fs = sapply(asscenarios, function(x) { setNames(list(getFeatureStepNames(x)), x$desc$scenario_id) })
reg = runLlamaModels(asscenarios, feature.steps.list = fs, pre = normalize,
  learners = learners, par.sets = par.sets, rs.iters = 2L, n.inner.folds = 2L)

# testJob(reg, 6, external = TRUE)

# jobs should be run with 2gig mem
# run time of all jobs
# summary(getJobInfo(reg)$time.running)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
      # 9      18      30     161      50    6320
# can be run on SLURM in a few hours in total

stop("we dont auto submit :)")

submitJobs(reg, resources = list(memory = 2048))
waitForJobs(reg)

d = reduceResultsExperiments(reg, strings.as.factors = FALSE, impute.val = list(succ = 0, par10 = Inf, mcp = Inf))
# save2(file = "llama_results.RData", res = d)

