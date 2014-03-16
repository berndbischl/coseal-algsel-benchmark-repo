\name{runLlamaModels}
\alias{runLlamaModels}
\title{Creates a registry which can be used for running several Llama models on a cluster.}
\usage{
runLlamaModels(astasks, baselines, classifiers, regressors, clusterers,
  add.feature.costs = TRUE)
}
\arguments{
  \item{astasks}{[\code{list}]\cr List of algorithm
  selection tasks (\code{\link{ASTask}}).}

  \item{baselines}{[\code{character}]\cr Vector of
  characters, defining the baseline models.  Default is
  c("vbs", "singleBest", "singleBestByPar",
  "singleBestBySuccesses").}

  \item{classifiers}{[\code{character}]\cr Vector of
  characters, defining the classification models.  Default
  is c("AdaBoost", "BayesNet", "IBk", "OneR",
  "MultilayerPerceptron", "RandomTree", "ctree", "fnn",
  "J48", "JRip", "kknn", "ksvm", "naiveBayes", "nnet",
  "randomForest", "rpart", "svm").}

  \item{regressors}{[\code{character}]\cr Vector of
  characters, defining the regression models.  Default is
  c("REPTree", "earth", "ksvm", "lm", "nnet",
  "randomForest", "rpart").}

  \item{clusterers}{[\code{character}]\cr Vector of
  characters, defining the cluster models.  Default is
  c("XMeans", "EM", "FarthestFirst", "SimpleKMeans").}

  \item{add.feature.costs}{[\code{logical(1)}]\cr If costs
  for features are present in runtime tasks, should they be
  added to the algorithm costs (because in realilty you
  would have to pay them)? Whether the algorithm hit the
  cutoff runtime is also newly calculated in this case.
  This adding of feature costs is never done for the
  baseline, but only for proper prognostic models.  Default
  is \code{TRUE}.}
}
\value{
registry.
}
\description{
It is likely that you need to install some additional R
packages for this from CRAN or extra Weka learner. The
latter can be one via e.g. \code{WPM("install-package",
"XMeans")}.
}

