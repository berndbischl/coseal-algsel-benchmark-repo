\name{convertToLlama}
\alias{convertToLlama}
\title{Convert an ASTask task object to a llama data object.}
\usage{
convertToLlama(astask, measure, feature.steps, add.feature.costs = TRUE)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to use
  for modelling.  Default is first measure in task.}

  \item{feature.steps}{[\code{character}]\cr Which feature
  steps are allowed?  Default is all steps.}

  \item{add.feature.costs}{[\code{logical(1)}]\cr If costs
  for features are present in runtime tasks, should they be
  added to the algorithm costs (because in reality you
  would have to pay them)? Whether the algorithm hit the
  cutoff runtime is also newly calculated in this case.
  This adding of feature costs is never done for the
  baseline, but only for proper prognostic models.  Default
  is \code{TRUE}.}
}
\value{
Result of calling \code{\link[llama]{input}}.
}
\description{
For stochastic algorithms and features, mean values are
computed across repetitions.
}

