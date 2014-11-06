# helper to convert feats to llama or mlr
convertFeats = function(asscenario, feature.steps, with.instance.id) {
  # reduce to inst + rep + allowed features
  # note that feats is ordered by instance, then repetition
  allowed.features = getProvidedFeatures(asscenario, feature.steps)
  feats = asscenario$feature.values
  feats = feats[, c("instance_id", "repetition", allowed.features), drop = FALSE]

  # aggregate features, only do this if repeated measurements to save time
  if (max(feats$repetition) > 1L) {
    feats = ddply(feats, c("instance_id"), function(d) {
      colMeans(d[, allowed.features, drop = FALSE])
    })
  } else {
    feats$repetition = NULL
  }

  # FIXME:
  # remove constant features, currently we do not count NAs as an extra-level
  # the feature would still be completely constant if we impute just with mean
  # THIS CHANGES IF WE CREATE  DUMMIES FOR NAs LIKE WE SHOULD!
  feats = removeConstScenFeats(feats)

  # impute feature values
  feats = impute(feats, target = character(0), classes = list(numeric = imputeMean()))$data
  if (!with.instance.id)
    feats$instance_id = NULL
  return(feats)
}
