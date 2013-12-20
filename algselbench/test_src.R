library(devtools)
library(foreign)
library(stringr)
library(plyr)
load_all(".")

dataset.dir = "~/cos/coseal/data/sat11-indu"

astask = parseASTask(dataset.dir)
summarizeFeatureRunStatus(astask)


# exploreAlgos(astask)

# print(plotAlgoPerfHist(astask, "TIME"))

