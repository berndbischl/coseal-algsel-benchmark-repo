#' Checks the feature data set for duplicated instances.
#'
#' Potentially duplicated instances are detected by grouping all instances
#' with equal feature vectors.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{list} of \code{character}]. List of instance id vectors where
#'   corresponding feature vectors are the same. Only groups of at least 2 elements are returned.
#' @export
checkDuplicatedInstances = function(asscenario) {
  checkArg(asscenario, "ASScenario")
  #FIXME: how do we handle stochastic features / repetions? think again about it.
  data = asscenario$feature.values
  # only take first repetition, might be wrong...
  data = subset(data, data$repetition == 1L)
  # remove instance_id
  iid =  data$instance_id
  data$instance_id = NULL
  # paste all feature values to get faster
  data2 = convertMatrixType(as.matrix(data), "character")
  data2 = apply(data2, 1, collapse, sep = "_")
  # get first instance with equal features
  first.equal = as.integer(sapply(data2, function(x) which(x == data2)[1L]))
  # split ids and filter out length 1 groups
  dups = setNames(split(iid, first.equal), NULL)
  Filter(function(x) length(x) > 1L, dups)
}
