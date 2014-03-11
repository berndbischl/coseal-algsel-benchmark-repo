\name{runLlamaModels}
\alias{runLlamaModels}
\title{Creates a registry which can be used for running several Llama models on a cluster.}
\usage{
runLlamaModels(astasks, baselines, classifiers, regressors, clusterers)
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
}
\value{
registry.
}
\description{
Creates a registry which can be used for running several
Llama models on a cluster.
}

