context("getCV")

test_that("getCV", {
  expect_equal(getNumberOfCVFolds(testscenario1), 10L)
  expect_equal(getNumberOfCVReps(testscenario1), 1L)
})
