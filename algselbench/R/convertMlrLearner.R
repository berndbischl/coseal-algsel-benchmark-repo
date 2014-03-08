library(mlr)
library(llama)

predict.LlamaClassifMlrWrapper = function(model, newdata, ...) {
  mlr:::predict.WrappedModel(model, newdata = newdata, ...)$data$response
}

convertMlrClassifLearnerToLlama = function(learner) {
  force(learner)
  function(formula, data) {
    theterms = terms(as.formula(formula), data=get("data"), envir=environment(formula))
    y = eval(attr(theterms, "variables")[[2]], envir=environment(formula)) 
    mydata = cbind(..best..=y, data)
    task = makeClassifTask(target="..best..", data=mydata)
    f = as.character(formula)
    features = f[-(1:2)]
    if (features != ".") {
      task = subsetTask(task, features = features)
    }
    m = train(learner, task)
    m = addClasses(m, "LlamaClassifMlrWrapper")
    return(m)
  }
}

rpartWrapperClassif = convertMlrClassifLearnerToLlama(makeLearner("classif.rpart"))
nnetWrapperClassif = convertMlrClassifLearnerToLlama(makeLearner("classif.nnet", size = 3L))
# earthWrapperRegr = convertMlrLearnerToLlama(makeLearner("regr.earth"))
# rpartWrapperRegr = convertMlrLearnerToLlama(makeLearner("regr.rpart"))
# nnetWrapperRegr = convertMlrLearnerToLlama(makeLearner("regr.nnet", size = 3L))
# svmWrapperClassif = convertMlrLearnerToLlama(makeLearner("classif.ksvm"))
# svmWrapperRegr = convertMlrLearnerToLlama(makeLearner("regr.ksvm"))

# data(satsolvers)
# trainTest = cvFolds(satsolvers, nfolds = 2L)
# res = classify(classifier = wrapper, data = trainTest)
