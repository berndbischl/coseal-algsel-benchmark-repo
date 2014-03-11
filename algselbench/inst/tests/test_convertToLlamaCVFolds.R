context("convertToLlamaCVFolds")

test_that("convertToLlamaCVFolds", {
  library(RWeka)
  folds = createCVSplits(testtask1, folds = 2L, reps = 1L)
  llama.task = convertToLlamaCVFolds(testtask1, folds)
  res = classify(classifier=J48, data=llama.task)
  
  folds = createCVSplits(testtask1, folds = 2L, reps = 2L)
  llama.task = convertToLlamaCVFolds(testtask1, folds)
  res = classify(classifier=J48, data=llama.task)
})


