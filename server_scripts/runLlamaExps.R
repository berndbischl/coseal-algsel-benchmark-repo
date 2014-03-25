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
ids = ids[seq(1, length(ids), 3)]
summarizeExperiments(reg = reg, ids = ids, show = c("algo", "prob", "model"))
submitJobs(reg, ids = ids, resources = list(walltime = 8 * 3600))
ids = findExperiments(reg = reg, algo.pattern="regr")
ids = ids[seq(3, length(ids), 4)]
summarizeExperiments(reg = reg, ids = ids, show = c("algo", "prob", "model"))
submitJobs(reg)
showStatus(reg)
d = reduceResultsExperiments(reg)
print(d)
save2(file = "llama_results_10folds.RData", res = d)


d = reduceResultsExperiments(reg, ids = c(findExperiments(reg, algo.pattern = "classif"),
  findExperiments(reg, algo.pattern = "cluster")))

save(d, file = "exp_results_newImputation_teil2.RData")

d1 = load2(file = "exp_results_newImputation_teil1.RData")
d2 = d

d = rbind(d1, d2)
d = d[order(d$prob, d$algo), ]
save(d, file = "llama_results.RData")


g = split(d[, c("prob", "model", "succ", "par10", "mcp")], d$prob)
g2 = lapply(g, function(data) {
  i1 = which(data$succ %in% sort(data$succ[-(1:4)], decreasing = TRUE)[1:3])
  i2 = which(data$par10 %in% sort(data$par10[-(1:4)], decreasing = FALSE)[1:3])
  i3 = which(data$mcp %in% sort(data$mcp[-(1:4)], decreasing = FALSE)[1:3])
  return(data[sort(unique(c(1:4, i1, i2, i3))), ])
})
