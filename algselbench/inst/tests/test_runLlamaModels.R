context("runLlamaModels")

test_that("runLlamaModels", {
  cv = createCVSplits(testscenario1, folds = 2, reps = 1L)
  fs = setNames(list(getFeatureStepNames(testscenario1)), testscenario1$desc$scenario_id)
  reg = runLlamaModels(testscenario1, feature.steps.list = fs,
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
