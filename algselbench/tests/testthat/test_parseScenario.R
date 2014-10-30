context("parseScenario")

test_that("parseASScenario", {
  expect_true(inherits(testscenario1, "ASScenario"))
})

test_that("parseASScenario checks feature runstatus levels", {
  expect_error(parseASScenario(file.path(INST_DIR, "broken-status-levels")),
    "Feature runstatus file contains illegal levels:")
})

test_that("parseASScenario verifies that all instances appear in CV", {
  expect_error(parseASScenario(file.path(INST_DIR, "broken-cv"), "Fold allocations given for"))
})
