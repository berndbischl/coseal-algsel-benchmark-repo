#FIXME: what do we do if we have reps here????

#' Return wether an instance was presolved and which step did it.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param feature.steps [\code{character}]\cr
#'   Which feature steps are allowed?
#'   Default is all steps.
#' @return [\code{list}]. In the following, \code{n} is the number of instances. All following object are ordered by \dQuote{instance_id}.
#'   \item{is.presolved [\code{logical(n)}]}{Was instance presolved? Named by instance ids.}
#'   \item{solve.steps [\code{character(n)}]}{Which step solved it? NA if no step did it. Named by instance ids.}
#'   \item{costs [\code{numeric(n)}]}{Feature costs for using the steps. Named by instance ids. NULL if no costs are present.}
#' @export
getCostsAndPresolvedStatus = function(asscenario, feature.steps) {
  assertClass(asscenario, "ASScenario")
  allsteps = getFeatureStepNames(asscenario)
  if (missing(feature.steps))
    feature.steps = allsteps
  else
    assertSubset(feature.steps, allsteps)
  frs = asscenario$feature.runstatus
  #FIXME:
  stopifnot(max(frs$repetition) == 1L)
  # note that frs and costs are ordered by instance_id
  iids = frs$instance_id
  # reduce to allowed feature steps
  frs = frs[, feature.steps, drop = FALSE]
  isps = frs == "presolved"
  ps = apply(isps, 1, any)
  # steps are in correct order of execution after parseASScenario
  # now get the first step that solves as index, or NA
  solve.steps1 = apply(isps, 1, function(x)
    ifelse(any(x), which.first(x), NA_integer_))
  # set NA to nr of steps (means we use all in costs)
  solve.steps2 = ifelse(is.na(solve.steps1), length(feature.steps), solve.steps1)
  if (!is.null(asscenario$feature.costs)) {
    costs = asscenario$feature.costs[, feature.steps, drop = FALSE]
    # FIXME: is this really ok??? we just dont know the costs if we got NA. check this
    costs[is.na(costs)] = 0
    # add up costs to solving step (or add up all)
    costs = sapply(seq_row(costs), function(i) sum(costs[i, 1:solve.steps2[i]]))
    costs = setNames(costs, iids)
  } else {
    costs = NULL
  }
  list(
    is.presolved = setNames(ps, iids),
    solve.steps = setNames(feature.steps[solve.steps1], iids),
    costs = costs
  )
}
