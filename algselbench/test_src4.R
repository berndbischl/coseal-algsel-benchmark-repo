library(llama)
library(RWeka)
library(mlr)

load_all()

# ast = parseASScenario("/home/bischl/cos/coseal/data/sat12-rand")
ast = parseASScenario("/home/bischl/cos/coseal/data/QBF-2011")


doNestedCVWithTuning = function(asscenario, ldf, timeout, learner, par.set, llama.fun, pre,
  maxit = 10L, n.inner.folds = 2L) {

  n.outer.folds = length(ldf$test)
  outer.preds = vector("list", n.outer.folds)

  for (i in 1:n.outer.folds) {
    print(i)
    ldf2 = ldf
    ldf2$data = ldf$train[[i]]
    ldf3 = cvFolds(ldf2, nfolds = n.inner.folds, stratify = FALSE)
    parvals = tuneLlamaModel(asscenario, learner, par.set, ldf3, llama.fun, pre, maxit)

    # now fit only on outer trainining set with best params and predict outer test set
    learner2 = setHyperPars(learner, par.vals = parvals)
    outer.split.ldf = ldf
    outer.split.ldf$train = list(ldf$train[[i]])
    outer.split.ldf$test = list(ldf$test[[i]])
    outer.preds[[i]] = llama.fun(learner2, data = outer.split.ldf, pre = pre)
  }
  retval = outer.preds[[1]]
  retval$predictions = lapply(outer.preds, function(x) { x$predictions[[1]] })
  return(retval)
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

llama.cv = convertToLlamaCVFolds(ast)
z = doNestedCVWithTuning(ast, llama.cv, 100, learner, ps, classify, pre = normalize, maxit = 2L)

# z = tuneLlamaModel(learner, ps, llama.cv, classify, pre = normalize, maxit = 2L)
print(successes(llama.cv, z))


