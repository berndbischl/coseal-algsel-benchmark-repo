library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(algselbench)  
}
test_dir("inst/tests")

