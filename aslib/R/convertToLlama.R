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
  # subset to features used in default steps
  tosel = intersect(names(feats), as.vector(unlist(asscenario$desc$feature_steps[feature.steps])))
  feats = feats[c("instance_id", tosel)]
  cp = convertPerf(asscenario, measure = measure, feature.steps = feature.steps,
    add.feature.costs = FALSE, with.instance.id = TRUE)

  if(!is.null(asscenario$feature.costs)) {
      # set all unknown feature costs (i.e. for feature steps that didn't run) to 0
      asscenario$feature.costs[is.na(asscenario$feature.costs)] = 0
      costs = list(groups=asscenario$desc$feature_steps[feature.steps],
          values=asscenario$feature.costs[,c("instance_id", feature.steps)])
      ldf = input(feats, cp$perf, successes = cp$successes,
          minimize = as.logical(!asscenario$desc$maximize[measure]), costs = costs)
  } else {
      ldf = input(feats, cp$perf, successes = cp$successes,
          minimize = as.logical(!asscenario$desc$maximize[measure]))
  }

  # LLAMA set the best algorithm for instances that were not solved by anything to NA,
  # set those to the single best solver over the entire set
  sb = as.character(singleBest(ldf)[1,]$algorithm)
  for(i in seq_along(ldf$best)) {
    if(all(sapply(ldf$best[[i]], is.na))) {
        ldf$best[[i]] = sb
    }
  }

  return(ldf)
}

#' Bakes presolving stuff into a LLAMA data frame.
#'
#' Determines whether any of the feature groups in the LLAMA data frame
#' presolve any of the instances. If so, the performances of all algorithms
#' in the portfolio are set to the runtime of the first used feature group
#' that presolves the respective instance. Furthermore, the success of all
#' algorithms on those instances is set to true.
#'
#' These modifications are done on the main LLAMA data and on any test splits.
#' They are *not* done on the training data. This function should only ever be
#' used to evaluate the performance of an actual selector that uses features
#' (i.e. not VBS or single best). Using it in polite company is to be avoided.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param ldf [\code{LLAMA data frame}]\cr
#'   LLAMA data frame to modify.
#' @return The LLAMA data frame with presolving baked into the algorithm
#'        performances.
#' @export
fixFeckingPresolve = function(asscenario, ldf) {
    presolvedGroups = names(asscenario$feature.runstatus)[apply(asscenario$feature.runstatus, 2, function(x) { any(x == "presolved") })]
    usedGroups = subset(names(asscenario$desc$feature_steps), sapply(names(asscenario$desc$feature_steps),
        function(x) { length(intersect(asscenario$desc$feature_steps[[x]], ldf$features)) > 0 }))
    # are we using any of the feature steps that cause presolving?
    if(length(intersect(presolvedGroups, usedGroups)) > 0) {
        presolved = asscenario$feature.runstatus[apply(subset(asscenario$feature.runstatus, TRUE, usedGroups), 1, function(x) { any(x == "presolved") }),]
        if(nrow(presolved) > 0) {
            presolvedTimes = sapply(rownames(presolved), function(x) {
                pCosts = subset(asscenario$feature.costs[x,], T, presolved[x,] == "presolved")
                pCosts = pCosts[intersect(names(pCosts), usedGroups)]
                pCosts[is.na(pCosts)] = 0
                as.numeric(pCosts[1])
            })
            rows = na.omit(match(presolved$instance_id, ldf$data$instance_id))
            if(length(rows) > 0) {
                ts = presolvedTimes[na.omit(match(ldf$data$instance_id, presolved$instance_id))]
                ldf$data[rows,ldf$performance] = ts
                ldf$data[rows,ldf$success] = T
                if(length(ldf$test) > 0) {
                    for(i in 1:length(ldf$test)) {
                        rows = na.omit(match(presolved$instance_id, ldf$data[ldf$test[[i]],]$instance_id))
                        if(length(rows) > 0) {
                            ts = presolvedTimes[na.omit(match(ldf$data[ldf$test[[i]],]$instance_id, presolved$instance_id))]
                            ldf$data[ldf$test[[i]],][rows,ldf$performance] = ts
                            ldf$data[ldf$test[[i]],][rows,ldf$success] = T
                        }
                    }
                }
            }
        }
    }
    return(ldf)
}
