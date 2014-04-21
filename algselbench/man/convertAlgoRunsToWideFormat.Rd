\name{convertAlgoRunsToWideFormat}
\alias{convertAlgoRunsToWideFormat}
\title{Converts \code{algo.runs} object of a task to wide format.}
\usage{
convertAlgoRunsToWideFormat(desc, algo.runs, measure)
}
\arguments{
  \item{desc}{[\code{\link{ASTaskDesc}}]\cr Description
  object of task.}

  \item{algo.runs}{[\code{data.frame}]\cr Algo runs
  data.frame from task.}

  \item{measure}{[\code{character(1)}]\cr Selected
  performance measure.  Default is first measure in task.}
}
\value{
[\code{data.frame}].
}
\description{
The first 2 columns are \dQuote{instance_id} and
\dQuote{repetition}. The remaining ones are the measured
performance values. The feature columns are in the same
order as \dQuote{algorithms_deterministic},
\dQuote{algorithms_stochastic} in the description object.
code{NA} means the performance value is not available,
possibly because the algorithm run was aborted. The
data.frame is sorted by \dQuote{instance_id}, then
\dQuote{repetition}.
}

