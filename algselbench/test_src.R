library(devtools)
library(foreign)
library(stringr)
library(plyr)
load_all("..")

#dataset.dir = "~/cos/coseal/data/sat11-indu"
dataset.dir = "../../../coseal/data/sat11-hand/"

astask = parseASTask(dataset.dir)
summarizeFeatureRunStatus(astask)

summarizeAlgoRuns(astask)
summarizeFeatureValues(astask)
dominatedAlgos(astask)
checkDuplicates(astask)
uselessInstances(astask)

# exploreAlgos(astask)

# print(plotAlgoPerfHist(astask, "TIME"))

