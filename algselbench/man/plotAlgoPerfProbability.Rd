\name{plotAlgoPerfProbability}
\alias{plotAlgoPerfProbability}
\title{Plot cumulative distribution function of performance values per algorithm across all instances.}
\usage{
  plotAlgoPerfProbability(astask, measure, log = FALSE,
    na.impute = TRUE)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to plot.
  Default is first measure in task.}

  \item{log}{[\code{logical(1)}]\cr Should the performance
  values be log10-transformed in the plot?  Default is
  FALSE.}

  \item{na.impute}{[\code{logical(1)}]\cr Should the values
  of algorithm runs with non-ok runstatus (missing
  performance values) be imputed? If yes, imputation is
  done via max + scalar * (max - min) for minimization
  problems and via min - scalar * (max - min) for
  maximization problems.  Default is TRUE.}
}
\value{
  ggplot2 plot object.
}
\description{
  Plot cumulative distribution function of performance
  values per algorithm across all instances.
}

