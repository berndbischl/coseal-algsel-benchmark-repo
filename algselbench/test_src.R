library(devtools)
library(foreign)
library(stringr)
library(plyr)
library(llama)
library(RWeka)
library(mlr)
library(BatchExperiments)
load_all(".")

#dataset.dir = "~/cos/coseal/data/sat11-indu"
ds.dirs = list.files("../../coseal/data/", full.names = TRUE)
# dataset.dir = "../../coseal/data/qbf_2011/"

astasks = lapply(ds.dirs[4:4], parseASTask)
reg = runLlamaModels(astasks, nfolds=2L)
submitJobs(reg)
waitForJobs(reg)
showStatus(reg)
d = reduceResultsExperiments(reg)
print(d)
# parallelStartSocket(4L)
# for (ds in ds.dirs) {
  # print(ds)
  # astask = parseASTask(dataset.dir)
  # d = runLlamaModels(astask, nfolds=2L)
  # print(d)
# }
# parallelStop()

