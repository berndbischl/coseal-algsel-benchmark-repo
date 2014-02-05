test_that("runLlamaModels", {
  r1 = runLlamaModels(testtask1, nfolds = 10L, stratify = TRUE)
  r2 = runLlamaModels(testtask1, nfolds = 10L, stratify = FALSE)
})