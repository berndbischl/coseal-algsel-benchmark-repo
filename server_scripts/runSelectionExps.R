library(BBmisc)
library(devtools)
library(llama)
library(stringr)
library(BatchExperiments)
library(checkmate)
library(aslib)
source("defs.R")

source("searchSequential.R")
source("searchSequentialObjective.R")

ds.dirs = list.files(file.path(coseal.data.dir, "data"), full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "BBOB|MACHINE")]
# ds.dirs = ds.dirs[4]
print(ds.dirs)
#ds.dirs = ds.dirs[7]
asscenarios = lapply(ds.dirs, parseASScenario)

unlink("run_selection_exps-files", recursive = TRUE)
reg = makeRegistry("run_selection_exps", seed = 123,
  packages = c("llama", "mlr", "aslib", "BatchExperiments", "parallelMap"),
  src.files = c("searchSequential.R", "searchSequentialObjective.R")
)


# FIXME: we need to store the names of all features and solvers in the result in the correct order!
batchMap(reg, fun = function(ast) {
  ctrl = makeSSControl(method = "sfs")
  ldf = convertToLlamaCVFolds(ast)
  n.bits = length(ldf$features)
  parallelStartMulticore(cpus = 16L)
  feats = searchSequential(searchSequentialObjectiveFeatures, n.bits, control = ctrl, scenario = ast, ldf = ldf,
    llama.model.fun = regression, mlr.learner = makeLearner("regr.randomForest"))
  n.bits = length(ldf$performance)
  solvs = searchSequential(searchSequentialObjectiveSolvers, n.bits, control = ctrl, scenario = ast, ldf = ldf,
    llama.model.fun = regression, mlr.learner = makeLearner("regr.randomForest"))
  parallelStop()
  list(id = ast$desc$scenario_id, feats = feats, solvs = solvs)
}, asscenarios)

submitJobs(reg)
waitForJobs(reg)
library(BatchExperiments)

# enrich results with all feat names and solver names posthoc, see FIXME above...
reg = loadRegistry("run_selection_exps-files")
res = loadResults(reg)
for (i in 1:length(res)) {
  r = res[[i]]
  ast = Filter(function(ast) ast$desc$scenario_id == r$id, asscenarios)[[1L]]
  ldf = convertToLlamaCVFolds(ast)
  r$all.feats = ldf$features
  r$all.solvers = ldf$performance
  res[[i]] = r
}

save2(file = "selection_results.RData", res = res)
