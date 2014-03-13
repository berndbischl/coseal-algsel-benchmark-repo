\name{plotAlgoScattermatrix}
\alias{plotAlgoScattermatrix}
\title{Creates a scatterplot matrix of the performance values of the algorithms.}
\usage{
  plotAlgoScattermatrix(astask, measure, log = FALSE)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to plot.
  Default is first measure in task.}

  \item{log}{[\code{logical(1)}]\cr Should the performance
  values be log10-transformed in the plot?  Default is
  FALSE.}
}
\value{
  plot object.
}
\description{
  Creates a scatterplot matrix of the performance values of
  the algorithms.
}

