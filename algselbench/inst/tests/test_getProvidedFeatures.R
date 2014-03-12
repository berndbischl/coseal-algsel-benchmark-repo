context("getProvidedFeatures")

test_that("getProvidedFeatures", {
  expect_equal(getProvidedFeatures(testtask1, steps = character(0)), character(0))
  expect_equal(getProvidedFeatures(testtask1), getFeatureNames(testtask1))

  # test with fake steps
  testtask = testtask1

  testtask$desc$feature_steps = list(s1 = c("a", "b"), s2 = c("b", "c"))
  testtask$desc$features_deterministic = c("a", "b", "c")
  testtask$desc$features_stochastic = character(0)
  expect_equal(getProvidedFeatures(testtask, "s1"), c("a"))
  expect_equal(getProvidedFeatures(testtask, "s2"), c("c"))
})
