library(BatchExperiments)
library(BBmisc)
library(plyr)

reg.tune = loadRegistry("run_llama_models-files-tune")
reg.notune = loadRegistry("run_llama_models-files-nottuned")

d.tune = reduceResultsExperiments(reg.tune, strings.as.factors = FALSE,
 impute.val = list(succ = 0, par10 = Inf, mcp = Inf))
d.notune = reduceResultsExperiments(reg.notune, strings.as.factors = FALSE,
 impute.val = list(succ = 0, par10 = Inf, mcp = Inf))

d = rbind.fill(d.tune, d.notune)

d$llama.fun = sapply(d$algo, function(a) {
  if(a == "baseline") return("foo")
  if(!is.na(pmatch("classif_", a))) return("classify")
  if(!is.na(pmatch("regr_", a))) return("regression")
  if(!is.na(pmatch("cluster_", a))) return("cluster")
  warning(paste("cannot match", a))
})

d$model = mapply(function(algo, type) {
  if(algo == "baseline") return(type)
  return(strsplit(algo, "_")[[1]][2])
}, d$algo, d$type)

d$algo = sapply(d$algo, function(a) {
  return(strsplit(a, "_")[[1]][1])
})

res = subset(d, select = c("id", "prob", "algo", "llama.fun", "model", "repl", "succ", "par10", "mcp"))
res$id = 1:nrow(res)
res = sortByCol(res, c("prob", "algo", "llama.fun"))

save(res, file = "llama_results.RData")
