library(methods)
library(devtools)
library(testthat)
library(llama)

if (interactive()) {
  load_all(".")
} else {
  library(algselbench)  
}
test_dir("inst/tests")

