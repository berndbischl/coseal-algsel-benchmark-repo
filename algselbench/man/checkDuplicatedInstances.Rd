\name{checkDuplicatedInstances}
\alias{checkDuplicatedInstances}
\title{Checks the feature data set for duplicated instances.}
\usage{
checkDuplicatedInstances(asscenario)
}
\arguments{
  \item{asscenario}{[\code{\link{ASScenario}}]\cr Algorithm
  selection scenario.}
}
\value{
[\code{list} of \code{character}]. List of instance id
vectors where corresponding feature vectors are the same.
Only groups of at least 2 elements are returned.
}
\description{
Potentially duplicated instances are detected by grouping
all instances with equal feature vectors.
}

