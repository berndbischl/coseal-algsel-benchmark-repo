library(methods)
library(devtools)
library(testthat)
library(llama)
library(mlr)
library(BatchExperiments)

if (interactive()) {
  load_all(".")
} else {
  library(algselbench)  
}
test_dir("inst/tests")

