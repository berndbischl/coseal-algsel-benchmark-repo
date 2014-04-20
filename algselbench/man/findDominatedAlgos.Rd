\name{findDominatedAlgos}
\alias{findDominatedAlgos}
\title{Creates a table that shows the dominance of one algorithm over another one.}
\usage{
findDominatedAlgos(astask, measure, reduce = FALSE, type = "logical")
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure for
  algorithm performance.  Default is first measure in
  task.}

  \item{reduce}{[\code{logical(1)}]\cr Should the resulting
  matrix be reduced to algorithms that a are either
  dominated by or dominate another algorithm?  Default is
  \code{FALSE}.}

  \item{type}{[\code{character(1)}]\cr Data type of the
  result object.\cr \dQuote{logical}: Logical matrix, TRUE
  means row algorithm dominates column algorithm.\cr
  \dQuote{character}: Same information but more
  human-readable. States how the row relates to the
  column.}
}
\value{
[\code{matrix}]. See above.
}
\description{
If NAs occur, they are imputed (before aggregation) by
\code{base + 0.3 * range}. \code{base} is the cutoff value
for runtimes tasks with cutoff or the worst performance for
all others.

Stochastic replications are aggregated by the mean value.
}

