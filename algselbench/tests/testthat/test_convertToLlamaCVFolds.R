context("convertToLlamaCVFolds")

test_that("convertToLlamaCVFolds", {
  folds = createCVSplits(testscenario1, folds = 2L, reps = 1L)
  llama.scenario = convertToLlamaCVFolds(testscenario1, cv.splits = folds)
  res = classify(classifier = makeLearner("classif.J48"), data = llama.scenario)

  # FIXME: re-add when llama can handle more than 1 rep
  # folds = createCVSplits(testscenario1, folds = 2L, reps = 2L)
  # llama.scenario = convertToLlamaCVFolds(testscenario1, cv.splits = folds)
  # res = classify(classifier = makeLearner("classif.J48"), data = llama.scenario)
})

test_that("convertToLlamaCVFolds check matching", {
  folds = createCVSplits(testscenario1, folds = 2L, reps = 1L)
  llama.scenario = convertToLlamaCVFolds(testscenario1, cv.splits = folds)
  for (i in unique(folds$fold)) {
    iid1 = as.character(llama.scenario$test[[i]]$instance_id)
    iid2 = as.character(subset(folds, folds$fold == i)$instance_id)
    expect_true(setequal(iid1, iid2))
  }
})
