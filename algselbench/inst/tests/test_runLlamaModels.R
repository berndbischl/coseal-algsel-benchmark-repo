context("runLlamaModels")

test_that("runLlamaModels", {
  r1 = runLlamaModels(testtask1, nfolds = 2L, stratify = TRUE)
  r2 = runLlamaModels(testtask1, nfolds = 2L, stratify = FALSE)
})
