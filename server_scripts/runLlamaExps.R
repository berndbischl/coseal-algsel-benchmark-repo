library(BBmisc)
library(devtools)
library(llama)
library(stringr)
library(mlr)
load_all("../algselbench")
source("defs.R")

ds.dirs = list.files(file.path(coseal.svn.dir, "data"), full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "BBOB|MACHINE")][6]
print(ds.dirs)
# ds.dirs = ds.dirs[1]
asscenarios = lapply(ds.dirs, parseASScenario)

# untuned learners, so we can see whether tuning helped
classif.untuned = list(
  makeLearner("classif.rpart"),
  makeLearner("classif.randomForest"),
  makeLearner("classif.ksvm")
)

regr.untuned = list(
  makeLearner("regr.lm"),
  makeLearner("regr.rpart"),
  makeLearner("regr.randomForest"),
  makeLearner("regr.mars")
)

cluster.untuned = list(
  makeLearner("cluster.XMeans")
)

# tuned learners
# FIXME: the whole code here is crappy prototype!!!
tune.svm = makeLearner("classif.ksvm")
ps.svm = makeParamSet(
  makeNumericParam("C",     lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
ps.rf = makeParamSet(
  makeIntegerParam("ntree", lower = 10, upper = 1000),
  makeIntegerParam("mtry", lower = 1, upper = 10)
)

makeTuneW = function(cl, ps, measures) {
  ctrl = makeTuneControlRandom(maxit = 10L)
  inner = makeResampleDesc("CV", iters = 2L)
  makeTuneWrapper(cl, resampling = inner, measures = measures, par.set = ps, control = ctrl)
}

classif.tuned = list(
  makeTuneW("classif.ksvm", ps.svm, mmce),
  makeTuneW("classif.randomForest", ps.rf, mmce)
)

regr.tuned = list(
  makeTuneW("regr.randomForest", ps.rf, mse)
)


# merge tuned and untuned learners to one list
classif = c(classif.untuned, classif.tuned)
regr = c(regr.untuned, regr.tuned)
cluster = cluster.untuned


fs = sapply(asscenarios, function(x) { setNames(list(getFeatureStepNames(x)), x$desc$scenario_id) })
reg = runLlamaModels(asscenarios, feature.steps.list = fs, pre = normalize,
  classifiers = classif, regr = regr, clusterers = cluster)

# reg = runLlamaModels(asscenarios, feature.steps.list = fs,
  # classifiers = list(makeLearner("classif.rpart")))

# jobs should be run with 2gig mem
# run time of all jobs
# summary(getJobInfo(reg)$time.running)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
      # 9      18      30     161      50    6320
# can be run on SLURM in a few hours in total

# stop("we dont auto submit :)")
submitJobs(reg, resources = list(memory = 2048))
waitForJobs(reg)

d = reduceResultsExperiments(reg, strings.as.factors = FALSE,
  impute.val = list(succ = 0, par10 = Inf, mcp = Inf))
save2(file = "llama_results.RData", res = d)

