\name{plotAlgoPerfDensities}
\alias{plotAlgoPerfDensities}
\title{Plot densities of performance values per algorithm across all instances.}
\usage{
plotAlgoPerfDensities(astask, measure, log = FALSE)
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
ggplot2 plot object.
}
\description{
Plot densities of performance values per algorithm across
all instances.
}

