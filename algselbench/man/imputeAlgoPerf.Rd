\name{imputeAlgoPerf}
\alias{imputeAlgoPerf}
\title{Imputes algorithm runs which have NA performance values.}
\usage{
  imputeAlgoPerf(astask, measure, base, range.scalar = 0.3,
    jitter = 0, structure = "algo.perf")
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to
  impute.  Default is first measure in task.}

  \item{base}{[\code{numeric(1)}]\cr See formula.  Default
  is maximum of performance values if measure should be
  minimized, or minimum for maximization case.}

  \item{range.scalar}{[\code{numeric(1)}]\cr See formula.
  Default is 0.3.}

  \item{jitter}{[\code{numeric(1)}]\cr See formula.
  Default is 0.}

  \item{structure}{[\character(1)}]\cr What structure
  should the result have?  Possible values are
  \dQuote{algo.runs} and \dQuote{algo.perf}.  They
  correspond to the structures in \code{\link{ASTask}}.
  Default is \dQuote{algo.perf}.}
}
\value{
  See \code{struture}.
}
\description{
  The following formula is used for imputation
  \code{range.span = max - min} \code{base +- range.scalar
  * range.span + N(0, sd = jitter * range.span)}. jitter is
  a
}

