context("parseTask")

test_that("parseASTask", {
  expect_true(inherits(testtask1, "ASTask"))
})

test_that("parseASTask checks feature runstatus levels", {
    expect_error(parseASTask("../broken-status-levels"),
        "Feature runstatus file contains illegal levels:")
})

test_that("parseASTask verifies that all instances appear in CV", {
    expect_error(parseASTask("../broken-cv"), "Fold allocations given for")
})
