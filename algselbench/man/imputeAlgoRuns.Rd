\name{imputeAlgoRuns}
\alias{imputeAlgoRuns}
\title{Imputes algorithm runs which have NA performance values.}
\usage{
imputeAlgoRuns(astask, measure, base = NULL, range.scalar = 0.3,
  jitter = 0)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to
  impute.  Default is first measure in task.}

  \item{base}{[\code{numeric(1)}]\cr See formula.  Default
  is \code{NULL}, which means maximum of performance values
  if measure should be minimized, or minimum for
  maximization case.}

  \item{range.scalar}{[\code{numeric(1)}]\cr See formula.
  Default is 0.3.}

  \item{jitter}{[\code{numeric(1)}]\cr See formula.
  Default is 0.}
}
\value{
[\code{data.frame}].
}
\description{
The following formula is used for imputation: \code{base +-
range.scalar * range.span + N(0, sd = jitter *
range.span)}\cr With \code{range.span = max - min}.

Returns an object like \code{algo.runs} of \code{astask},
but drops the runstatus and all other measures.
}

