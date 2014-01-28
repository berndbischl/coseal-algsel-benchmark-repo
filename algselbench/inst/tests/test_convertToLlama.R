test_that("convertToLlama", {
  task = parseASTask("../qbf_2011")
  llama.task = convertToLlama(task)
  cv = cvFolds(llama.task)
  res = classify(classifier=J48, data=cv)
})


