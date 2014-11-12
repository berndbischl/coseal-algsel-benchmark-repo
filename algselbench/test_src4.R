library(llama)
library(RWeka)
library(mlr)

load_all()

# ast = parseASScenario("/home/bischl/cos/coseal/data/sat12-rand")
# ast = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011")



doNestedCVWithTuning = function(asscenario, learner, par.set, ldf, llama.fun, pre, maxit = 10L) {
  # desc = asscenario$desc
  # cutoff = desc$algorithm_cutoff_time
  # timeout = if (desc$performance_type[[1L]] == "runtime" && !is.na(cutoff))
    # cutoff
  # else
    # NULL

  for (i in 1:length(llama.cv)) {
    print(i)
    ldf2 = ldf
    ldf2$data = ldf$train[[i]]
    ldf3 = cvFolds(ldf2)
    parvals = tuneLlamaModel(asscenario, learner, par.set, ldf3, llama.fun, pre, maxit)
    learner = setHyperPars(learner, par.vals = parvals)
    p = llama.fun(learner, data = ldf, pre = pre)
    ldf4 = fixFeckingPresolve(asscenario, ldf3)
    par10 = mean(parscores(ldf4, p, timeout = timeout))
  }
}

tuneLlamaModel = function(asscenario, learner, par.set, cv.splits, llama.fun, pre, maxit = 10) {
  timeout = 100
  des = generateRandomDesign(maxit, par.set)
  des.list = dfRowsToList(des, par.set)
  ys = vnapply(des.list, function(x) {
    print(x)
    learner = setHyperPars(learner, par.vals = x)
    p = llama.fun(learner, data = cv.splits, pre = pre)
    ldf = fixFeckingPresolve(asscenario, cv.splits)
    par10 = mean(parscores(ldf, p, timeout = timeout))
  })
  print(ys)
  best.i = getMinIndex(ys)
  best.parvals = des.list[[best.i]]
  return(best.parvals)
}

learner = makeLearner("classif.rpart")
ps = makeParamSet(
  makeIntegerParam("minsplit", lower = 1, upper = 20)
)

llama.cv = convertToLlamaCVFolds(ast, add.feature.costs = FALSE)
z = tuneLlamaModel(learner, ps, llama.cv, classify, pre = normalize, maxit = 2L)
print(z)



