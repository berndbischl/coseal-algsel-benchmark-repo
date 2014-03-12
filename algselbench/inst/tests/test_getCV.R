context("getCV")

test_that("getCV", {
  g = getNumberOfCVFolds(testtask1)
  expect_equal(g, 10L)
  
  g = getNumberOfCVReps(testtask1)
  expect_equal(g, 5L)
})