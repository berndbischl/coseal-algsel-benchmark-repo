library(foreign)
library(stringr)
library(plyr)

source("parseDescription.R")
source("parseAlgorithmRuns.R")
source("parseASTask.R")
source("eda_algos.R")

dataset.dir = "~/cos/coseal/data/maxsat12-pms"

# astask = parseASTask(dataset.dir)
# print(astask)

# exploreAlgos(astask)

print(plotAlgoPerfHist(astask, "TIME"))


