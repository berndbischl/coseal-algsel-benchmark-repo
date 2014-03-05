\name{plotAlgoCorMatrix}
\alias{plotAlgoCorMatrix}
\title{Plots the correlation matrix of the algorithms.}
\usage{
plotAlgoCorMatrix(astask, measure, order.method, hclust.method, cor.method)
}
\arguments{
  \item{astask}{[\code{\link{ASTask}}]\cr Algorithm
  selection task.}

  \item{measure}{[\code{character(1)}]\cr Measure to plot.
  Default is first measure in task.}

  \item{order.method}{[\code{character(1)}]\cr Method for
  ordering the algorithms within the plot.  Possible values
  are \code{hclust} (for hierarchical clustering order),
  \code{FPC} (first principal component order), \code{AOE}
  (angular order of eigenvectors), \code{original}
  (original order) and \code{alphabet} (alphabetical
  order). Default is \code{hclust}.}

  \item{hclust.method}{[\code{character(1)}]\cr Method for
  hierarchical clustering. Only useful, when
  \code{order.method} is set to \code{hclust}. Possible
  values are \code{ward}, \code{single}, \code{complete},
  \code{average}, \code{mcquitty}, \code{median} and
  \code{centroid}.  Default is \code{ward}.}

  \item{cor.method}{[\code{character(1)}]\cr Method to be
  used for calculating the correlation between the
  algorithms.  Possible values are \code{pearson},
  \code{kendall} and \code{spearman}.  The default is
  \code{pearson}.}
}
\value{
corrplot
}
\description{
Plots the correlation matrix of the algorithms.
}

