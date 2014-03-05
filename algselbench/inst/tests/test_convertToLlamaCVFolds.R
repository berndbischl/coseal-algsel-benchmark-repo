context("convertToLlamaCVFolds")

test_that("convertToLlamaCVFolds", {
  folds = createCVSplits(testtask1, folds = 2L, reps = 1L)
  llama.task = convertToLlamaCVFolds(testtask1, folds)
  library(RWeka)
  res = classify(classifier=J48, data=llama.task)
})


