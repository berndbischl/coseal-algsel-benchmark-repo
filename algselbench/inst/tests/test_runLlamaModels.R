context("runLlamaModels")

test_that("runLlamaModels", {
  cv = createCVSplits(testtask1, folds = 2, reps = 1L)
  reg = runLlamaModels(testtask1,
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
