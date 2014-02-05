\name{convertToLlama}
\alias{convertToLlama}
\title{Convert an ASTask task object to a llama data object.}
\usage{
convertToLlama(astask, measure)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to use
  for modelling.  Default is first measure in task.}
}
\value{
Result of calling \code{\link[llama]{input}}.
}
\description{
For stochastic algorithms and features, mean values are
computed over the repetitions.
}

