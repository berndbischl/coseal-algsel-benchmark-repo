test_that("findDominatedAlgos", {
  f1 = findDominatedAlgos(testtask1, "relERT")
  f2 = findDominatedAlgos(testtask1, "ERT")
  f3 = findDominatedAlgos(testtask1)
})