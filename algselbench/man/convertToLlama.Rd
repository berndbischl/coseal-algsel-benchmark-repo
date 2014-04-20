\name{convertToLlama}
\alias{convertToLlama}
\alias{convertToMlr}
\title{Convert an ASTask task object to a llama data object.}
\usage{
convertToLlama(astask, measure, feature.steps, add.feature.costs = TRUE)

convertToMlr(astask, measure, feature.steps, add.feature.costs = TRUE)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to use
  for modeling.  Default is first measure in task.}

  \item{feature.steps}{[\code{character}]\cr Which feature
  steps are allowed?  Default are the default feature steps
  or all steps in case no defaults were defined.}

  \item{add.feature.costs}{[\code{logical(1)}]\cr If costs
  for features are present in runtime tasks, should they be
  added to the algorithm costs (because in reality you
  would have to pay them)? Whether the algorithms hit the
  cutoff runtime is also recalculated in this case.  Adding
  the feature costs should not be done for the baseline
  models, but only for proper prognostic models.  If no
  costs are present, 0 is added as cost and a warning is
  issued.  Default is \code{TRUE}.}
}
\value{
Result of calling \code{\link[llama]{input}}.
}
\description{
For features, mean values are computed across repetitions.
For algorithms, repetitions are not supported at the moment
and will result in an error.
}

