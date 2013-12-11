\name{plotAlgoPerfDensities}
\alias{plotAlgoPerfDensities}
\title{Plot densities of performance values per algorithm across all instances.}
\usage{
plotAlgoPerfDensities(astask, measure)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to plot.
  Default is first measure in task.}
}
\value{
ggplot2 plot object.
}
\description{
Plot densities of performance values per algorithm across
all instances.
}

