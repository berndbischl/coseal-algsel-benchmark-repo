library(BBmisc)
library(devtools)
library(RWeka)
library(llama)
library(stringr)
load_all("../algselbench")
source("defs.R")

ds.dirs = list.files(file.path(coseal.svn.dir, "data"), full.names = TRUE)
ds.dirs = ds.dirs[!str_detect(ds.dirs, "bbob|machine")]
print(ds.dirs)
astasks = lapply(ds.dirs, parseASTask)
astasks = lapply(astasks, addDefaultsToASTask, "../configs/")

classifiers.weka = c("meta/AdaBoostM1", "bayes/BayesNet", "lazy/IBk", "rules/OneR",
  "trees/RandomTree", "trees/J48", "rules/JRip")
classifiers.mlr = c("classif.ctree", "classif.ksvm", "classif.naiveBayes",
  "classif.randomForest", "classif.rpart")

reg = runLlamaModels(astasks, classifiers=c(classifiers.weka, classifiers.mlr), 
  clusterers = c("EM", "FarthestFirst", "SimpleKMeans"), pre = normalize)

# jobs should be run with 2gig mem
# run time of all jobs
# summary(getJobInfo(reg)$time.running)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      # 9      18      30     161      50    6320 
# can be run on SLURM in a few hours in total

# submitJobs(reg, resources = list(memory = 2048))

d = reduceResultsExperiments(reg)
save2(file = "llama_results.RData", res = d)

