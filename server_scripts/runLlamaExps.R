library(BBmisc)
library(devtools)
library(RWeka)
library(llama)
library(stringr)
load_all("../algselbench")
source("defs.R")
source("eda_config.R")

ds.dirs = list.files(file.path(coseal.svn.dir, "data"), full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "BBOB|MACHINE")]
print(ds.dirs)
# ds.dirs = ds.dirs[1]
astasks = lapply(ds.dirs, parseASTask)
configs = lapply(astasks, readEDAConfig, confpath = "../configs")
feature.steps.list = extractSubList(configs, "feature.steps.default", simplify = FALSE)
names(feature.steps.list) = sapply(astasks, function(x) x$desc$task_id)

reg = runLlamaModels(astasks, feature.steps.list = feature.steps.list,

  classifiers = c(
    "meta/AdaBoostM1", "bayes/BayesNet", "lazy/IBk", "rules/OneR",
    "trees/RandomTree", "trees/J48", "rules/JRip",

    "classif.ctree", "classif.ksvm", "classif.naiveBayes", "classif.randomForest", "classif.rpart"
  ),

  regressors = c("regr.lm", "regr.rpart", "regr.randomForest", "regr.earth"),

  clusterers = c("EM", "FarthestFirst", "SimpleKMeans"),

  pre = normalize
)

# reg = runLlamaModels(astasks, feature.steps.list = feature.steps.list,
  # classifiers = "classif.rpart")

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

