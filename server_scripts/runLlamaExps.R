library(algselbench)

# dataset.dir = "../../coseal/data/csp_2010"
# ast = parseASTask(dataset.dir)
# lt = convertToLlama(ast)

ds.dirs = list.files("../coseal/data", full.names = TRUE)
astasks = lapply(ds.dirs, parseASTask)
reg = runLlamaModels(astasks, nfolds=2L)
submitJobs(reg)
waitForJobs(reg)
showStatus(reg)
d = reduceResultsExperiments(reg)
print(d)
save2(file = "llama_results.RData", res = d)
