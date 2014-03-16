#FIXME: what do we do if we have reps here????

#' Return wether an instance was presolved and which step did it.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param feature.steps [\code{character}]\cr
#'   Which feature steps are allowed?
#'   Default is all steps.
#' @return [\code{list}]. In the following, \code{n} is the number of instances.
#'   \item{is.presolved [\code{logical(n)}]}{Was instance presolved? Named by instance ids.}
#'   \item{solve.steps [\code{character(n)}]}{Which step solved it? NA if no step did it. Named by instance ids.}
#'   \item{costs [\code{numeric(n)}]}{Feature costs for using the steps. Named by instance ids.}
#' @export
getPresolved = function(astask, feature.steps) {
  checkArg(astask, "ASTask")
  allsteps = getFeatureStepNames(astask)
  if (missing(feature.steps))
    feature.steps = allsteps
  else
    checkArg(feature.steps, subset = allsteps)
  frs = astask$feature.runstatus
  #FIXME:
  stopifnot(max(frs$repetition) == 1L)
  iids = frs$instance_id
  # reduce to allowed feature steps
  frs = frs[, feature.steps, drop = FALSE]
  costs = astask$feature.costs[, feature.steps, drop = FALSE]
  isps = frs == "presolved"
  ps = apply(isps, 1, any)
  # steps are in correct order of execution after parseASTask
  # now get the first step that solves as index, or NA
  solve.steps1 = apply(isps, 1, function(x)
    ifelse(any(x), which.first(x), NA_integer_))
  # set NA to nr of steps (means we use all in costs)
  solve.steps2 = ifelse(is.na(solve.steps1), length(feature.steps), solve.steps1)
  # add up costs to solving step (or add up all)
  costs = sapply(seq_row(costs), function(i) sum(costs[i, 1:solve.steps2[i]]))
  list(
    is.presolved = setNames(ps, iids),
    solve.steps = setNames(solve.steps1, iids),
    costs = setNames(costs, iids)
  )
}
