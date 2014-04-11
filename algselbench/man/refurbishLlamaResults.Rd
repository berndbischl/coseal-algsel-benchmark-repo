\name{refurbishLlamaResults}
\alias{refurbishLlamaResults}
\title{Refurbishes the llama experiments data frame by coloring the best values.}
\usage{
refurbishLlamaResults(data, color.best = "#FF0000",
  color.best.in.group = "##FFA500")
}
\arguments{
  \item{data}{[\code{data.frame}]\cr Data frame retrieved
  from the llama experiments.}

  \item{color.best}{[\code{character(1)}]\cr HTML-Color
  used for the three best values within a performance
  measurement.}

  \item{color.best.in.group}{[\code{character(1)}]\cr
  HTML-Color used for highlighting the best performance per
  group.}
}
\value{
[\code{data.frame}].
}
\description{
Refurbishes the llama experiments data frame by coloring
the best values.
}

