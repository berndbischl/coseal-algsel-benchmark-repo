\name{findUselessInstances}
\alias{findUselessInstances}
\title{Checks algorithm and feature data sets for useless instances.}
\usage{
findUselessInstances(astask, measure)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure that's
  been used for analyzing the algorithm performances.
  Default is first measure in task.}
}
\value{
[\code{list}(3)].  List of data frames, which give an
overview of instances that consist of only one single value
(and eventually some NAs) per instance.  There output
consists of three data frames: one for the algo runs, the
feature values and the feature runstatus, respectively.
}
\description{
Checks algorithm and feature data sets for useless
instances.
}

