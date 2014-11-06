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
      # set all unknown feature costs (i.e. for feature steps that didn't run) to 0
      asscenario$feature.costs[is.na(asscenario$feature.costs)] = 0
      costs = list(groups=asscenario$desc$feature_steps[feature.steps],
          values=asscenario$feature.costs[,names(asscenario$feature.costs)[-2]])
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

fixFeckingPresolve = function(asscenario, ldf) {
    presolvedGroups = names(asscenario$feature.runstatus)[apply(asscenario$feature.runstatus, 2, function(x) { any(x == "presolved") })] 
    usedGroups = subset(names(asscenario$desc$feature_steps), sapply(names(asscenario$desc$feature_steps),
        function(x) { length(intersect(asscenario$desc$feature_steps[[x]], ldf$features)) > 0 }))
    # are we using any of the feature steps that cause presolving?
    if(length(intersect(presolvedGroups, usedGroups)) > 0) {
        presolved = asscenario$feature.runstatus[apply(asscenario$feature.runstatus, 1, function(x) { any(x == "presolved") }),]
        if(nrow(presolved) > 0) {
            presolvedTimes = sapply(rownames(presolved), function(x) {
                pCosts = asscenario$feature.costs[x,presolved[x,] == "presolved"]
                pCosts[is.na(pCosts)] = 0
                as.numeric(pCosts[1])
            })
            rows = match(presolved$instance_id, ldf$data$instance_id)
            ldf$data[rows,ldf$performance] = presolvedTimes
            ldf$data[rows,ldf$success] = T
            if(length(ldf$test) > 0) {
                for(i in 1:length(ldf$test)) {
                    rows = na.omit(match(presolved$instance_id, ldf$test[[i]]$instance_id))
                    if(length(rows) > 0) {
                        ts = presolvedTimes[na.omit(match(ldf$test[[i]]$instance_id, presolved$instance_id))]
                        ldf$test[[i]][rows,ldf$performance] = ts
                        ldf$test[[i]][rows,ldf$success] = T
                    }
                }
            }
        }
    }
    return(ldf)
}
