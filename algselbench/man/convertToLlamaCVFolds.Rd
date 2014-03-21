\name{convertToLlamaCVFolds}
\alias{convertToLlamaCVFolds}
\title{Convert an ASTask task object to a llama data object with cross-validation folds.}
\usage{
  convertToLlamaCVFolds(astask, measure, feature.steps,
    add.feature.costs = TRUE, cv.splits)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to use
  for modelling.  Default is first measure in task.}

  \item{feature.steps}{[\code{character}]\cr Which feature
  steps are allowed?  Default are the default feature steps
  or all steps in case no defaults were defined.}

  \item{add.feature.costs}{[\code{logical(1)}]\cr See
  \code{\link{convertToLlama}}.  Default is \code{TRUE}.}

  \item{cv.splits}{[\code{data.frame}]\cr Data frame
  defining the split of the data into cross-validation
  folds, as returned by \code{\link{createCVSplits}}.
  Default are the splits \code{astask$cv.splits}}
}
\value{
  Result of calling \code{\link[llama]{input}} with data
  partitioned into folds.
}
\description{
  For stochastic algorithms and features, mean values are
  computed across repetitions.
}

