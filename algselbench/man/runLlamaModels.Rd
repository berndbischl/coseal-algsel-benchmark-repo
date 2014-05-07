\name{runLlamaModels}
\alias{runLlamaModels}
\title{Creates a registry which can be used for running several Llama models on a cluster.}
\usage{
runLlamaModels(asscenarios, feature.steps.list, baselines,
  classifiers = character(0), regressors = character(0),
  clusterers = character(0), pre)
}
\arguments{
  \item{asscenarios}{[\code{list}]\cr List of algorithm
  selection scenarios (\code{\link{ASScenario}}).}

  \item{feature.steps}{[\code{list} of \code{character}]\cr
  Named list of feature steps we want to use.  Must be
  named with scenario ids.}

  \item{baselines}{[\code{character}]\cr Vector of
  characters, defining the baseline models.  Default is
  c("vbs", "singleBest", "singleBestByPar",
  "singleBestBySuccesses").}

  \item{classifiers}{[\code{character}]\cr Vector of
  characters, defining the classification models.  Default
  is none.}

  \item{regressors}{[\code{character}]\cr Vector of
  characters, defining the regression models.  Default is
  none.}

  \item{clusterers}{[\code{character}]\cr Vector of
  characters, defining the cluster models.  Default is
  none.}

  \item{pre}{[\code{function}]\cr A function (e.g.
  normalize) to preprocess the feature data.  By default no
  preprocessing is done.}
}
\value{
BatchExperiments registry.
}
\description{
It is likely that you need to install some additional R
packages for this from CRAN or extra Weka learner. The
latter can be one via e.g. \code{WPM("install-package",
"XMeans")}.

Feature costs are added for real prognostic models but not
for baseline models.

Machine learning models are run with their default
settings. For the clusterers we set the number of clusters
(N) to the number of algorithms in the scenario. Except for
XMeans where we set the maximum number of clusters (H) in
the same way, as a parameter for the number of clusters
does not exist.
}

