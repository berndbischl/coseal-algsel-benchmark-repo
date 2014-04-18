\name{addDefaultsToASTask}
\alias{addDefaultsToASTask}
\title{Integrates default settings from the configs file to the description object.}
\usage{
  addDefaultsToASTask(astask, conf.path)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Description
  object for the algorithm selection task.}

  \item{conf.path}{[\code{character(1)}]\cr Path to configs
  directory of benchmark data set.}
}
\value{
  [\code{\link{ASTask}}]. Description object.
}
\description{
  Integrates default settings from the configs file to the
  description object.
}

