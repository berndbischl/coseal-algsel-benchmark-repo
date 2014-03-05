\name{convertToLlamaCVFolds}
\alias{convertToLlamaCVFolds}
\title{Convert an ASTask task object to a llama data object with cross-validation folds.}
\usage{
convertToLlamaCVFolds(astask, folds, measure)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{folds}{[\code{data.frame(1)}]\cr Data frame
  defining the split of the data into cross-validation
  folds, as returned by \code{\link{createCVSplits}}.}

  \item{measure}{[\code{character(1)}]\cr Measure to use
  for modelling.  Default is first measure in task.}
}
\value{
Result of calling \code{\link[llama]{input}} with data
partitioned into folds.
}
\description{
For stochastic algorithms and features, mean values are
computed across repetitions.
}

