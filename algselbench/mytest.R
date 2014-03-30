library(methods)
library(devtools)
library(testthat)
library(llama)
library(mlr)
load_all(".")

ast = parseASTask("/home/bischl/cos/coseal/data/csp_2010")

lt = convertToLlamaCVFolds(ast)
lrn = makeLearner("classif.rpart")
res = classify(convertMlrLearnerToLlama(lrn), lt)
mcp = misclassificationPenalties(lt, res$predictions)
print(mean(unlist(mcp)))
