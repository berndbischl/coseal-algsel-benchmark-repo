\name{calcAggrPerfValues}
\alias{calcAggrPerfValues}
\title{Calculate aggregated performances per algorithm across all instances.}
\usage{
  calcAggrPerfValues(astask, aggr)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{aggr}{[\code{function}]\cr Agregation function.
  Default is \code{mean}}
}
\value{
  [\code{data.frame}]. Rows correspond to algorithms and
  are named in this fashion.  For each performance measure
  two columns are created: One for the aggregated
  performance values, one for the percentage of missing
  values.
}
\description{
  Calculate aggregated performances per algorithm across
  all instances.
}

