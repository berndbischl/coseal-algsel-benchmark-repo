#' @export
predict.LlamaClassifMlrWrapper = function(object, newdata, ...) {
  mlr:::predict.WrappedModel(object, newdata = newdata, ...)$data$response
}

#' @export
convertMlrLearnerToLlama = function(learner) {
  force(learner)
  function(formula, data) {
    theterms = terms(as.formula(formula), data=get("data"), envir=environment(formula))
    y = eval(attr(theterms, "variables")[[2]], envir=environment(formula))
    mydata = cbind(..best..=y, data)
    scenario = if (learner$type == "classif")
      makeClassifScenario(target="..best..", data = mydata)
    else
      makeRegrScenario(target="..best..", data = mydata)
    f = as.character(formula)
    features = f[-(1:2)]
    # FIXME: does not work for subset of features
    if (features != ".") {
      scenario = subsetScenario(scenario, features = features)
    }
    m = train(learner, scenario)
    m = addClasses(m, "LlamaClassifMlrWrapper")
    return(m)
  }
}

