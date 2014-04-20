\name{summarizeAlgoRuns}
\alias{summarizeAlgoRuns}
\title{Creates summary data.frame for algorithm performance values across all instances.}
\usage{
summarizeAlgoRuns(astask, measure)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Selected measure.
  Default is first measure in task.}
}
\value{
[\code{data.frame}].
}
\description{
Creates summary data.frame for algorithm performance values
across all instances.
}

