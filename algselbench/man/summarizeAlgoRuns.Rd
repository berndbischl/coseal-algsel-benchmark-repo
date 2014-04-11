\name{summarizeAlgoRuns}
\alias{summarizeAlgoRuns}
\title{Creates a table that gives an overview of the performance values
per algorithm across all instances.}
\usage{
summarizeAlgoRuns(astask, measure)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure that's
  been used for analyzing the algorithm performances.
  Default is first measure in task.}
}
\value{
[\code{data.frame}].  Data frame, which gives an overview
of the performance values.
}
\description{
Creates a table that gives an overview of the performance
values per algorithm across all instances.
}

