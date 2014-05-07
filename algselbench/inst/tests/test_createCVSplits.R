context("createCVSplits")

test_that("createCVSplits", {
  s = createCVSplits(testscenario1, folds = 2L, reps = 1L)
  expect_is(s, "data.frame")
  expect_equal(ncol(s), 3L)
  expect_equal(range(s$repetition), c(1, 1))
  expect_equal(range(s$fold), c(1, 2))
  expect_true(setequal(s$instance_id, getInstanceNames(testscenario1)))
  
  s = createCVSplits(testscenario1, folds = 5L, reps = 10L)
  expect_equal(range(s$repetition), c(1, 10))
  expect_equal(range(s$fold), c(1, 5))
  for (i in 1:10)
    expect_true(setequal(subset(s, repetition == i)$instance_id, getInstanceNames(testscenario1)))
})



