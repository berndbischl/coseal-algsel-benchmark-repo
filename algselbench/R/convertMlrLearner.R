#' @export
predict.LlamaClassifMlrWrapper = function(model, newdata, ...) {
  mlr:::predict.WrappedModel(model, newdata = newdata, ...)$data$response
}

#' @export
convertMlrLearnerToLlama = function(learner) {
  force(learner)
  function(formula, data) {
    theterms = terms(as.formula(formula), data=get("data"), envir=environment(formula))
    y = eval(attr(theterms, "variables")[[2]], envir=environment(formula))
    mydata = cbind(..best..=y, data)
    task = if (learner$type == "classif")
      makeClassifTask(target="..best..", data = mydata)
    else
      makeRegrTask(target="..best..", data = mydata)
    f = as.character(formula)
    features = f[-(1:2)]
    # FIXME: does not work for subset of features
    if (features != ".") {
      task = subsetTask(task, features = features)
    }
    m = train(learner, task)
    m = addClasses(m, "LlamaClassifMlrWrapper")
    return(m)
  }
}

