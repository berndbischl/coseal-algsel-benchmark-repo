context("convertToLlama")

test_that("convertToLlama", {
  llama.task = convertToLlama(testtask3, add.feature.costs = FALSE)
  expect_equal(llama.task$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.task$data$f1, testtask3$feature.values$f1)
  expect_equal(llama.task$data$f2, testtask3$feature.values$f2)
  expect_equal(llama.task$data$f3, testtask3$feature.values$f3)
  expect_equal(llama.task$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.task$data$a1_success, c(TRUE, TRUE, TRUE))
  expect_equal(llama.task$data$a2_success, c(TRUE, TRUE, FALSE))
  expect_equal(llama.task$data$a1, c(30, 90, 70))
  expect_equal(llama.task$data$a2, c(50, 30, 100))

  llama.task = convertToLlama(testtask3, add.feature.costs = TRUE)
  expect_equal(llama.task$data$a1_success, c(TRUE, TRUE, TRUE))
  expect_equal(llama.task$data$a2_success, c(TRUE, TRUE, FALSE))
  expect_equal(llama.task$data$a1, c(60, 30, 80))
  expect_equal(llama.task$data$a2, c(80, 30, 100))

  library(RWeka)
  llama.task = convertToLlama(testtask1, add.feature.costs = FALSE)
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)
  expect_warning({
    llama.task = convertToLlama(testtask1, add.feature.costs = TRUE)
  }, "Adding always 0")
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)

  llama.task = convertToLlama(testtask2, add.feature.costs = FALSE)
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)
  llama.task = convertToLlama(testtask2, add.feature.costs = TRUE)
  cv = cvFolds(llama.task, nfolds = 2L)
  res = classify(classifier = J48, data = cv)
})


test_that("convertToLlama parses real task correctly", {
  llama.task = convertToLlama(testtask1, add.feature.costs = FALSE)
  expect_equal(llama.task$performance, c("2clsQ", "quantor", "QuBE", "sKizzo", "sSolve"))
  expect_equal(llama.task$success, c("2clsQ_success", "quantor_success", "QuBE_success", "sKizzo_success", "sSolve_success"))
  expect_equal(llama.task$features, c( "EXIST_VARS", "FORALL_VARS",
        "TOTAL_VARS", "CLAUSES",
        "LITERALS", "EXIST_SET",
        "FORALL_SET", "TOTAL_SET",
        "UNARY_CLAUSES", "BINARY_CLAUSES",
        "TERNARY_MORE_CLAUSES", "POS_HORN",
        "NEG_HORN", "EXIST_LIT_PER_CLAUSE",
        "FORALL_LIT_PER_CLAUSE", "EXIST_VARS_PER_SET",
        "FORALL_POS_LITS_PER_CLAUSE", "FORALL_NEG_LITS_PER_CLAUSE",
        "OCCS_POS_NO_PER_VAR", "OCCS_FORALL_NO_PER_VAR",
        "OCCS_FORALL_POS_NO_PER_VAR", "W_OCCS_POS_NO_PER_VAR",
        "W_OCCS_FORALL_NO_PER_VAR", "W_OCCS_FORALL_POS_NO_PER_VAR",
        "W_PRODUCTS", "LITN_LIT",
        "LITEP_LIT", "LITEN_LITE",
        "LITEN_LITN", "LITFN_LIT",
        "LITFP_LITFN", "OCCP_OCCN",
        "OCCE_OCC", "OCCEN_OCC",
        "OCCFP_OCCF", "OCCEN_OCCE",
        "OCCEN_OCCN", "OCCFP_OCCFN",
        "TERMORE_CLAUSE", "NEG_HORN_CLAUSE",
        "WOCCN_WOCC", "WOCCEP_WOCC",
        "WOCCFN_WOCC", "WOCCEP_WOCCE",
        "WOCCEP_WOCCP", "WOCCFN_WOCCN"))
  expect_equal(length(llama.task$data$best), 1368)
})
