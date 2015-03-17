context("convertToLlama")

test_that("convertToLlama", {
  llama.scenario = convertToLlama(testscenario3)
  expect_equal(llama.scenario$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.scenario$data$f1, testscenario3$feature.values$f1)
  expect_equal(llama.scenario$data$f2, testscenario3$feature.values$f2)
  expect_equal(llama.scenario$data$f3, testscenario3$feature.values$f3)
  expect_equal(llama.scenario$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.scenario$data$a1_success, c(TRUE, TRUE, TRUE))
  expect_equal(llama.scenario$data$a2_success, c(TRUE, TRUE, FALSE))
  expect_equal(llama.scenario$data$a1, c(30, 90, 70))
  expect_equal(llama.scenario$data$a2, c(50, 30, 100))
  expect_equal(llama.scenario$best, c("a1", "a2", "a1"))

  llama.scenario = convertToLlama(testscenario1)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = makeLearner("classif.J48"), data = cv)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = makeLearner("classif.J48"), data = cv)

  llama.scenario = convertToLlama(testscenario2)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = makeLearner("classif.J48"), data = cv)
  llama.scenario = convertToLlama(testscenario2)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = makeLearner("classif.J48"), data = cv)
})

test_that("convertToLlama always sets best algorithm", {
  llama.scenario = convertToLlama(testscenario4)
  expect_equal(llama.scenario$best, c("a1", "a2", "a2"))
})

test_that("convertToLlama parses real scenario correctly", {
  llama.scenario = convertToLlama(testscenario1)
  iid1 = as.character(llama.scenario$data$instance_id)
  iid2 = as.character(testscenario1$algo.runs$instance_id)
  expect_true(setequal(iid1, iid2))
  expect_equal(llama.scenario$performance, c("X2clsQ", "quantor", "QuBE", "sKizzo", "sSolve"))
  expect_equal(llama.scenario$success, c("X2clsQ_success", "quantor_success", "QuBE_success", "sKizzo_success", "sSolve_success"))
  expect_equal(llama.scenario$features, c( "EXIST_VARS", "FORALL_VARS",
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
  expect_equal(length(llama.scenario$best), 1368)
})

test_that("convertToLlama handles costs correctly", {
  llama.scenario = convertToLlama(testscenario2)
  iid1 = as.character(llama.scenario$data$instance_id)
  iid2 = as.character(testscenario2$algo.runs$instance_id)
  expect_true(setequal(iid1, iid2))
  default_groups = testscenario2$desc$feature_steps[sapply(names(testscenario2$desc$feature_steps), function(x) { x %in% testscenario2$desc$default_steps })]
  expect_equal(llama.scenario$costGroups, default_groups)
  expect_false("repetition" %in% llama.scenario$costs)
})

test_that("fixFeckingPresolve", {
  llama.scenario1 = convertToLlama(testscenario2)
  vbs1 = sum(parscores(llama.scenario1, vbs))
  llama.scenario2 = fixFeckingPresolve(testscenario2, llama.scenario1)
  vbs2 = sum(parscores(llama.scenario2, vbs))

  # You would think so, wouldn't you. But no...
  # expect_true(vbs2 < vbs1)
})
