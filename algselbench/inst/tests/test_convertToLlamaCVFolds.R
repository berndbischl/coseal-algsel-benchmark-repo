context("convertToLlamaCVFolds")

test_that("convertToLlamaCVFolds", {
  library(RWeka)
  folds = createCVSplits(testtask1, folds = 2L, reps = 1L)
  llama.task = convertToLlamaCVFolds(testtask1, cv.splits = folds)
  res = classify(classifier = J48, data = llama.task)
  
  # FIXME: re-add when llama can handle more than 1 rep
  # folds = createCVSplits(testtask1, folds = 2L, reps = 2L)
  # llama.task = convertToLlamaCVFolds(testtask1, cv.splits = folds)
  # res = classify(classifier = J48, data = llama.task)
})


