\name{plotAlgoScattermatrix}
\alias{plotAlgoScattermatrix}
\title{Creates a scatterplot matrix of the performance values of the algorithms.}
\usage{
  plotAlgoScattermatrix(astask, measure, trafo = identity,
    na.impute = TRUE)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to plot.
  Default is first measure in task.}

  \item{trafo}{[\code{function(1)}]\cr Function applied to
  the data as preprocessing to the generation of the plot.
  Default is identity.}

  \item{na.impute}{[\code{logical(1)}]\cr Should the values
  of algorithm runs with non-ok runstatus (missing
  performance values) be imputed? If yes, imputation is
  done via max + 0.1 * (max - min) for minimization
  problems and via min - 0.1 * (max - min) for maximization
  problems.  Default is TRUE.}
}
\value{
  plot object.
}
\description{
  Creates a scatterplot matrix of the performance values of
  the algorithms.
}

