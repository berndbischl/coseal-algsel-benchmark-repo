library(devtools)
library(foreign)
library(stringr)
library(plyr)
load_all(".")

#dataset.dir = "~/cos/coseal/data/sat11-indu"
dataset.dir = "../../coseal/data/qbf_2011/"

astask = parseASTask(dataset.dir)

llama.task = convertToLlama(astask)
cv = cvFolds(llama.task)
