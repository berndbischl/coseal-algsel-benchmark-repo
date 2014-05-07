\name{imputeAlgoPerf}
\alias{imputeAlgoPerf}
\title{Imputes algorithm performance for runs which have NA performance values.}
\usage{
imputeAlgoPerf(asscenario, measure, base = NULL, range.scalar = 0.3,
  jitter = 0, impute.zero.vals = FALSE)
}
\arguments{
  \item{asscenario}{[\code{\link{ASScenario}}]\cr Algorithm
  selection scenario.}

  \item{measure}{[\code{character(1)}]\cr Measure to
  impute.  Default is first measure in scenario.}

  \item{base}{[\code{numeric(1)}]\cr See formula.  Default
  is \code{NULL}, which means maximum of performance values
  if measure should be minimized, or minimum for
  maximization case.}

  \item{range.scalar}{[\code{numeric(1)}]\cr See formula.
  Default is 0.3.}

  \item{jitter}{[\code{numeric(1)}]\cr See formula.
  Default is 0.}

  \item{impute.zero.vals}{[\code{logical(1)}]\cr Should
  values which are exactly 0 be imputed to 1e-6?  This
  allows to take the logarithm later on, handy for
  subsequent visualizations.  Note that this really only
  makes sense for non-negative measures!  Default is
  FALSE.}
}
\value{
[\code{data.frame}].
}
\description{
The following formula is used for imputation: \code{base +-
range.scalar * range.span + N(0, sd = jitter *
range.span)}\cr With \code{range.span = max - min}.

Returns an object like \code{algo.runs} of
\code{asscenario}, but drops the runstatus and all other
measures.
}

