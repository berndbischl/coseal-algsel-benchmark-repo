context("runLlamaModels")

test_that("runLlamaModels", {
  cv = createCVSplits(testtask1, folds = 2, reps = 1L)
  runLlamaModels(testtask1, 
    baselines = "vbs", 
    classifiers = "J48",
    regressors = character(0),
    clusterers = character(0)
  )
  submitJobs(reg)
  waitForJobs(reg)
  res = reduceResultsExperiments(reg)
  expect_true(is.data.frame(res) && nrow(res) == 2L)
})
