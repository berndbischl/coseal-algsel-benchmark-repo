# helper to convert feats to llama or mlr
convertFeats = function(astask, feature.steps, with.instance.id) {
  # reduce to inst + rep + allowed features
  # note that feats is ordered by instance, then repetition
  allowed.features = getProvidedFeatures(astask, feature.steps)
  feats = astask$feature.values
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
  feats = impute(feats, target = character(0), classes = list(numeric = imputeMean()))$data
  if (!with.instance.id)
    feats$instance_id = NULL
  return(feats)
}
