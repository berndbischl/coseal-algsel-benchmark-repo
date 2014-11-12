context("runLlamaModels")

test_that("runLlamaModels", {
  fs = setNames(list(getFeatureStepNames(testscenario1)), testscenario1$desc$scenario_id)
  reg = runLlamaModels(list(testscenario1), feature.steps.list = fs,
    baselines = "vbs",
    classifiers = makeLearner("classif.J48"),
    regressors = makeLearner("regr.rpart"),
    clusterers = makeLearner("cluster.SimpleKMeans")
  )
  submitJobs(reg)
  waitForJobs(reg)
  res = reduceResultsExperiments(reg)
  expect_true(is.data.frame(res) && nrow(res) == 4L)
  expect_true(abs(res[1,]$par10 - 8337.099) < .1)
})

test_that("runLlamaModels w/ costs", {
  fs = setNames(list(getFeatureStepNames(testscenario2)), testscenario2$desc$scenario_id)
  reg = runLlamaModels(list(testscenario2), feature.steps.list = fs,
    baselines = "vbs",
    classifiers = makeLearner("classif.OneR")
  )
  submitJobs(reg)
  waitForJobs(reg)
  res = reduceResultsExperiments(reg)
  expect_true(is.data.frame(res) && nrow(res) == 2L)
  expect_true(abs(res[1,]$par10 - 2221.497) < .1)
  # greater than without costs
  expect_true(res[2,]$par10 > 3274.425)
})
