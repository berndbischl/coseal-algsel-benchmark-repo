\name{plotAlgoPerf}
\alias{plotAlgoPerf}
\alias{plotAlgoPerfBoxplots}
\alias{plotAlgoPerfCDFs}
\alias{plotAlgoPerfDensities}
\alias{plotAlgoPerfScatterMatrix}
\title{EDA plots for performance values of algorithms across all instances.}
\usage{
plotAlgoPerfBoxplots(astask, measure, log = FALSE)

plotAlgoPerfCDFs(astask, measure, impute.zero.vals = FALSE, log = FALSE)

plotAlgoPerfDensities(astask, measure, impute.zero.vals = FALSE,
  log = FALSE)

plotAlgoPerfScatterMatrix(astask, measure, log = FALSE)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to plot.
  Default is first measure in task.}

  \item{impute.zero.vals}{[\code{logical(1)}]\cr Should
  values which are exactly 0 be imputed to 1e-6?  This
  allows to take the logarithm later on, handy for
  subsequent visualizations.  Note that this really only
  makes sense for non-negative measures!  Default is
  FALSE.}

  \item{log}{[\code{logical(1)}]\cr Should the performance
  values be log10-transformed in the plot?  Default is
  FALSE.}
}
\value{
ggplot2 plot object.
}
\description{
If NAs occur, they are imputed (before aggregation) by
\code{base + 0.3 range + jitter} . \code{base} is is the
cutoff value for runtimes tasks with cutoff or the worst
performance for all others.

Stochastic replications are aggregated by the mean value.
}

