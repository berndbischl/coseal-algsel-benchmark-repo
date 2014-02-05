test_that("plotAlgoScattermatrix", {
  p1 = plotAlgoScattermatrix(testtask1, "relERT", log = FALSE)
  p2 = plotAlgoScattermatrix(testtask1, "relERT", log = TRUE)
  p3 = plotAlgoScattermatrix(testtask1, "ERT", log = FALSE)
  p4 = plotAlgoScattermatrix(testtask1, "lERT", log = TRUE)
  p5 = plotAlgoScattermatrix(testtask1, log = FALSE)
  p6 = plotAlgoScattermatrix(testtask1, log = TRUE)
})