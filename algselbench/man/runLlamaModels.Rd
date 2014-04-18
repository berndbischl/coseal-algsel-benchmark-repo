\name{runLlamaModels}
\alias{runLlamaModels}
\title{Creates a registry which can be used for running several Llama models on a cluster.}
\usage{
  runLlamaModels(astasks, baselines, classifiers,
    regressors, clusterers, pre)
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

  Feature costs are added for real prognostic models but
  not for baseline models.
}

