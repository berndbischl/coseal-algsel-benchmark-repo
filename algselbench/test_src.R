library(devtools)
library(foreign)
library(stringr)
library(plyr)
library(llama)
library(RWeka)
library(mlr)
load_all(".")

#dataset.dir = "~/cos/coseal/data/sat11-indu"
dataset.dir = "../../coseal/data/qbf_2011/"

astask = parseASTask(dataset.dir)

# llama.task = convertToLlama(astask)
# cv = cvFolds(llama.task, nfolds = 2L)
parallelStartSocket(4L)
d = runLlamaModels(astask, nfolds=2L)
parallelStop()
print(d)
