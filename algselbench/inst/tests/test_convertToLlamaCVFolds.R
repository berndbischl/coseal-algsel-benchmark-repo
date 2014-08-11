context("convertToLlamaCVFolds")

test_that("convertToLlamaCVFolds", {
  library(RWeka)
  folds = createCVSplits(testscenario1, folds = 2L, reps = 1L)
  llama.scenario = convertToLlamaCVFolds(testscenario1, cv.splits = folds, add.feature.costs = FALSE)
  res = classify(classifier = makeLearner("classif.J48"), data = llama.scenario)

  # FIXME: re-add when llama can handle more than 1 rep
  # folds = createCVSplits(testscenario1, folds = 2L, reps = 2L)
  # llama.scenario = convertToLlamaCVFolds(testscenario1, cv.splits = folds)
  # res = classify(classifier = makeLearner("classif.J48"), data = llama.scenario)
})

test_that("convertToLlamaCVFolds check matching", {
  folds = createCVSplits(testscenario1, folds = 2L, reps = 1L)
  llama.scenario = convertToLlamaCVFolds(testscenario1, cv.splits = folds, add.feature.costs = FALSE)
  for(i in unique(folds$fold)) {
    expect_true(setequal(llama.scenario$test[[i]]$instance_id, subset(folds, folds$fold==i)$instance_id))
  }
})
