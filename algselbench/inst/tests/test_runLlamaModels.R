context("runLlamaModels")

test_that("runLlamaModels", {
  cv = createCVSplits(testtask1, folds = 2, reps = 1L)
  fs = setNames(list(getFeatureStepNames(testtask1)), testtask1$desc$task_id)
  reg = runLlamaModels(testtask1, feature.steps.list = fs,
    baselines = "vbs",
    classifiers = "trees/J48",
    regressors = "regr.rpart",
    clusterers = character(0)
  )
  submitJobs(reg)
  waitForJobs(reg)
  res = reduceResultsExperiments(reg)
  expect_true(is.data.frame(res) && nrow(res) == 3L)
})
