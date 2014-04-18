\name{shortenPathsInValidator}
\alias{shortenPathsInValidator}
\title{Refurbishes the llama experiments data frame by coloring the best values.}
\usage{
  shortenPathsInValidator(s, task.dir)
}
\arguments{
  \item{s}{[\code{list(2)}]\cr List, containing the output
  of the validator script.}

  \item{task.dir}{[\code{character(2)}]\cr Path to the
  algorithm task's directory.}
}
\value{
  [\code{list(2)}].\cr Same list as the input parameter,
  but without showing the full paths to the files within
  the task directory.
}
\description{
  Refurbishes the llama experiments data frame by coloring
  the best values.
}

