library(BBmisc)
library(devtools)
library(RWeka)
library(llama)
load_all("../algselbench")
source("defs.R")

ds.dirs = list.files(file.path(coseal.svn.dir, "data"), full.names = TRUE)
print(ds.dirs)
astasks = lapply(ds.dirs[-c(grep("bbob", ds.dirs), grep("machine", ds.dirs),
  grep("maxsat", ds.dirs))], parseASTask)
# astasks = lapply(ds.dirs[c(grep("/sat1", ds.dirs))], parseASTask)
astasks = lapply(astasks, addDefaultsToASTask, "../configs/")
# reg = runLlamaModels(astasks, baseline=c("vbs"),classifiers=c("classif.kknn"), 
#   regressors=c("regr.lm"), clusterers = c("XMeans"), pre = normalize)
classifiers.weka = c("meta/AdaBoostM1", "bayes/BayesNet", "lazy/IBk", "rules/OneR",
  "trees/RandomTree", "trees/J48", "rules/JRip")
classifiers.mlr = c("classif.ctree", "classif.ksvm", "classif.naiveBayes",
  "classif.randomForest", "classif.rpart")
reg = runLlamaModels(astasks, classifiers=c(classifiers.weka, classifiers.mlr), 
  clusterers = c("EM", "FarthestFirst", "SimpleKMeans"), pre = normalize)
ids = findExperiments(reg = reg, algo.pattern="cluster")
ids = findExperiments(reg = reg, algo.pattern="regr")
ids = ids[seq(3, length(ids), 4)]
submitJobs(reg)
showStatus(reg)
d = reduceResultsExperiments(reg)
print(d)
save2(file = "llama_results_10folds.RData", res = d)
