\name{findDominatedAlgos}
\alias{findDominatedAlgos}
\title{Creates a table that shows the dominance of one algorithm over another one.}
\usage{
findDominatedAlgos(astask, measure)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure that's
  been used for analyzing the algorithm performances.
  Default is first measure in task.}
}
\value{
[\code{data.frame}].  Overview of the dominance between the
algorithms.
}
\description{
Creates a table that shows the dominance of one algorithm
over another one.
}

