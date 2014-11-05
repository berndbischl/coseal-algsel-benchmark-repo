#' Convert an ASScenario scenario object to a llama data object.
#'
#' For features, mean values are computed across repetitions.
#' For algorithms, repetitions are not supported at the moment and will result in an error.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param measure [\code{character(1)}]\cr
#'   Measure to use for modeling.
#'   Default is first measure in scenario.
#' @param feature.steps [\code{character}]\cr
#'   Which feature steps are allowed?
#'   Default are the default feature steps or all steps
#'   in case no defaults were defined.
#' @return Result of calling \code{\link[llama]{input}}.
#' @export
convertToLlama = function(asscenario, measure, feature.steps) {
  ch = convertToCheck(asscenario, measure, feature.steps, TRUE)
  measure = ch$measure; feature.steps = ch$feature.steps

  feats = convertFeats(asscenario, with.instance.id = TRUE)
  cp = convertPerf(asscenario, measure = measure, feature.steps = feature.steps,
    add.feature.costs = FALSE, with.instance.id = TRUE)

  if(!is.null(asscenario$feature.costs)) {
      # FIXME: figure out how to do this properly
      asscenario$feature.costs[is.na(asscenario$feature.costs)] = 0
      costs = list(groups=asscenario$desc$feature_steps[feature.steps],
          values=asscenario$feature.costs)
      ldf = input(feats, cp$perf, successes = cp$successes,
          minimize = !asscenario$desc$maximize[measure], costs = costs)
  } else {
      ldf = input(feats, cp$perf, successes = cp$successes,
          minimize = !asscenario$desc$maximize[measure])
  }
  # LLAMA set the best algorithm for instances that were not solved by anything to NA,
  # set those to the single best solver over the entire set
  sb = as.character(singleBest(ldf)[[1]]$algorithm)
  for(i in seq_along(ldf$data$best)) {
    if(all(sapply(ldf$data$best[[i]], is.na))) {
        ldf$data$best[[i]] = sb
    }
  }

  return(ldf)
}

