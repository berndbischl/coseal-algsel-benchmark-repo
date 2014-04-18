\name{plotAlgoScattermatrix}
\alias{plotAlgoScattermatrix}
\title{Creates a scatterplot matrix of the performance values of the algorithms.}
\usage{
  plotAlgoScattermatrix(astask, measure, trafo = identity)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to plot.
  Default is first measure in task.}

  \item{trafo}{[\code{function(1)}]\cr Function applied to
  the data as preprocessing to the generation of the plot.
  Default is identity.}
}
\value{
  plot object.
}
\description{
  Creates a scatterplot matrix of the performance values of
  the algorithms.
}

