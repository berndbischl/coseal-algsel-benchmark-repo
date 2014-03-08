# library(algselbench)

# dataset.dir = "../../coseal/data/csp_2010"
# ast = parseASTask(dataset.dir)
# lt = convertToLlama(ast)

ds.dirs = list.files("../coseal/data", full.names = TRUE)
astasks = lapply(ds.dirs, parseASTask)
reg = runLlamaModels(astasks, nfolds = 10L)
time1 = proc.time()
submitJobs(reg)
waitForJobs(reg)
showStatus(reg)
time2 = proc.time()
print(time2 - time1)
d = reduceResultsExperiments(reg)
print(d)
save2(file = "llama_results_10folds.RData", res = d)
