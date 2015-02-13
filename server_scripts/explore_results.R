library(BatchExperiments)
library(plyr)
reg.tune = loadRegistry("run_llama_models-files-tune")

d.tune = reduceResultsExperiments(reg.tune, strings.as.factors = FALSE,
 impute.val = list(succ = 0, par10 = Inf, mcp = Inf))

reg.sel.rf = loadRegistry("run_selection_exps-files")
reg.sel.lm = loadRegistry("run_selection_exps-files-lm")

d.sel.rf = reduceResults(reg.sel.rf, init = data.frame(), fun = function(aggr, job, res) {
  rbind(aggr,
    data.frame(prob = res$id, algo = "sel-feats-rf", par10 = res$feats$y,
      nsel = sum(res$feats$x$x), nmax = length(res$feats$x$x)),
    data.frame(prob = res$id, algo = "sel-solvers-rf", par10 = res$solvs$y,
      nsel = sum(res$solvs$x$x), nmax = length(res$solvs$x$x))
  )
})
d.sel.lm = reduceResults(reg.sel.lm, init = data.frame(), fun = function(aggr, job, res) {
  rbind(aggr,
    data.frame(prob = res$id, algo = "sel-feats-lm", par10 = res$feats$y,
      nsel = sum(res$feats$x$x), nmax = length(res$feats$x$x)),
    data.frame(prob = res$id, algo = "sel-solvers-lm", par10 = res$solvs$y,
      nsel = sum(res$solvs$x$x), nmax = length(res$solvs$x$x))
  )
})

d = rbind.fill(d.tune, rbind(d.sel.lm, d.sel.rf))


