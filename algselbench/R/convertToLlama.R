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
#' @param add.feature.costs [\code{logical(1)}]\cr
#'   If costs for features are present in runtime scenarios, should they be added to the algorithm costs
#'   (because in reality you would  have to pay them)? Whether the algorithms hit the cutoff runtime
#'   is also recalculated in this case.
#'   Adding the feature costs should not be done for the
#'   baseline models, but only for proper prognostic models.
#'   If no costs are present, 0 is added as cost and a warning is issued.
#'   Default is \code{TRUE}.
#' @return Result of calling \code{\link[llama]{input}}.
#' @export
convertToLlama = function(asscenario, measure, feature.steps, add.feature.costs = TRUE) {
  ch = convertToCheck(asscenario, measure, feature.steps, add.feature.costs)
  measure = ch$measure; feature.steps = ch$feature.steps

  feats = convertFeats(asscenario, with.instance.id = TRUE)
  cp = convertPerf(asscenario, measure = measure, feature.steps = feature.steps,
    add.feature.costs = add.feature.costs, with.instance.id = TRUE)

  input(feats, cp$perf, successes = cp$successes, minimize = !asscenario$desc$maximize[measure])
}

