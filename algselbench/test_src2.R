library(llama)
library(RWeka)
# load_all("~/cos/mlr")
load_all(".")


ast = parseASTask("/home/bischl/cos/coseal/data/QBF_2011")
lt = convertToLlamaCVFolds(ast, add.feature.costs = F)
lres = classify(classifier = J48, data = lt)
mcps1 = unlist(misclassificationPenalties(lt, lres$predictions))
mcp1 = mean(mcps1)
print(summary(mcps1))
print(mcp1)
