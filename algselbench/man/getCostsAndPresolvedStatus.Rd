\name{getCostsAndPresolvedStatus}
\alias{getCostsAndPresolvedStatus}
\title{Return wether an instance was presolved and which step did it.}
\usage{
getCostsAndPresolvedStatus(astask, feature.steps)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{feature.steps}{[\code{character}]\cr Which feature
  steps are allowed?  Default is all steps.}
}
\value{
[\code{list}]. In the following, \code{n} is the number of
instances.  \item{is.presolved [\code{logical(n)}]}{Was
instance presolved? Named by instance ids.}
\item{solve.steps [\code{character(n)}]}{Which step solved
it? NA if no step did it. Named by instance ids.}
\item{costs [\code{numeric(n)}]}{Feature costs for using
the steps. Named by instance ids. NULL if no costs are
present.}
}
\description{
Return wether an instance was presolved and which step did
it.
}

