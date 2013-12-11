\name{calcAggrPerfValues}
\alias{calcAggrPerfValues}
\title{Calculate aggregated performances per algorithm across all instances.}
\usage{
calcAggrPerfValues(astask, aggr = median)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{aggr}{[\code{function}]\cr Agregation function.
  Default is \code{mean}}
}
\value{
[\code{data.frame}]. First column is algorithm names,
further columns are aggregated performance values.
}
\description{
Calculate aggregated performances per algorithm across all
instances.
}

