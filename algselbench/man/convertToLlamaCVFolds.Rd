\name{convertToLlamaCVFolds}
\alias{convertToLlamaCVFolds}
\title{Convert an ASTask task object to a llama data object with cross-validation folds.}
\usage{
convertToLlamaCVFolds(astask, measure, cv.splits)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to use
  for modelling.  Default is first measure in task.}

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

