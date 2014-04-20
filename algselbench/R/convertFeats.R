# helper to convert feats to llama or mlr
convertFeats = function(astask, feature.steps, with.id) {
  desc = astask$desc
  feats = astask$feature.values
  allowed.features = getProvidedFeatures(astask, feature.steps)
  
  # reduce to inst + rep + allowed features
  # note that feats is ordered by instance, then repetition
  feats = feats[, c("instance_id", "repetition", allowed.features), drop = FALSE]

  # aggregate features, only do this if repeated measurements to save time
  if (max(feats$repetition) > 1L) {
    feats = ddply(feats, c("instance_id"), function(d) {
      colMeans(d[, allowed.features, drop = FALSE])
    })
  } else {
    feats$repetition = NULL
  }

  # impute feature values
  # FIXME: why cant we impute without target
  # FIXME: check whether imputing the median is useful
#   cols = sapply(feats, function(x) any(is.na(x)) & (class(x) == "logical"))
#   feats = impute(feats, target = character(0), classes = list(numeric = imputeMax(2),
#     integer = imputeMax(2), character = imputeConstant("missing"),
#     factor = imputeConstant("missing"), logical = imputeMode()),
#     dummies = names(cols)[cols])$data
  cols = sapply(feats, function(x) any(is.na(x)))
  cols = names(cols)[cols]
#   cols = setNames(lapply(cols, function(x) imputeMedian()), cols)
  imputeMean = function () {
    makeImputeMethod(learn = function(data, target, col)
      mean(data[[col]], na.rm = TRUE), impute = mlr:::simpleImpute)
  }
  cols = setNames(lapply(cols, function(x) imputeMean()), cols)
  feats = impute(feats, target = character(0), cols = cols)$data
  if (!with.id)
    feats$instance_id = NULL
  return(feats)
}
