context("getCV")

test_that("getCV", {
  expect_equal(getNumberOfCVFolds(testtask1), 10L)
  expect_equal(getNumberOfCVReps(testtask1), 1L)
})
