context("getProvidedFeatures")

test_that("getProvidedFeatures", {
  expect_equal(getProvidedFeatures(testscenario1, steps = character(0)), character(0))
  expect_equal(getProvidedFeatures(testscenario1), getFeatureNames(testscenario1))

  # test with fake steps
  testscenario = testscenario1

  testscenario$desc$feature_steps = list(s1 = c("a", "b"), s2 = c("b", "c"))
  testscenario$desc$features_deterministic = c("a", "b", "c")
  testscenario$desc$features_stochastic = character(0)
  expect_equal(getProvidedFeatures(testscenario, "s1"), c("a"))
  expect_equal(getProvidedFeatures(testscenario, "s2"), c("c"))
})
