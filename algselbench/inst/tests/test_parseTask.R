test_that("parseTask", {
  task = parseASTask("../qbf_2011")
  expect_true(inherits(task, "ASTask"))
})


