context("convertToLlama")

test_that("convertToLlama", {
  llama.task = convertToLlama(testtask3, add.feature.costs = FALSE)
  expect_equal(llama.task$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.task$data$f1, testtask3$feature.values$f1)
  expect_equal(llama.task$data$f2, testtask3$feature.values$f2)
  expect_equal(llama.task$data$f3, testtask3$feature.values$f3)
  expect_equal(llama.task$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.task$data$a1_success, c(TRUE, TRUE, TRUE))
  expect_equal(llama.task$data$a2_success, c(TRUE, TRUE, FALSE))
  expect_equal(llama.task$data$a1, c(30, 90, 70))
  expect_equal(llama.task$data$a2, c(50, 30, 100))

  llama.task = convertToLlama(testtask3, add.feature.costs = TRUE)
  expect_equal(llama.task$data$a1_success, c(TRUE, TRUE, TRUE))
  expect_equal(llama.task$data$a2_success, c(TRUE, TRUE, FALSE))
  expect_equal(llama.task$data$a1, c(60, 30, 80))
  expect_equal(llama.task$data$a2, c(80, 30, 100))

  library(RWeka)
  llama.task = convertToLlama(testtask1, add.feature.costs = FALSE)
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)
  expect_warning({
    llama.task = convertToLlama(testtask1, add.feature.costs = TRUE)
  }, "Adding always 0")
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)

  llama.task = convertToLlama(testtask2, add.feature.costs = FALSE)
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)
  llama.task = convertToLlama(testtask2, add.feature.costs = TRUE)
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)
})


