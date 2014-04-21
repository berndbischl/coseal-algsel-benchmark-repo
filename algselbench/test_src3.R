library(llama)
library(RWeka)
load_all("~/cos/mlr")
load_all(".")

# ast = parseASTask("/home/bischl/cos/coseal/data/sat12-rand")
# ast = parseASTask("/home/bischl/cos/coseal/data/QBF_2011")

feats = ast$feature.values
cvs = ast$cv.splits

# print("data test")
# print(cvs[1, ])
# print(feats[feats$instance_id == "adder-6-sat-shuffled", 1:5])
# print("data train")
# print(subset(cvs, fold == 2)[1,])
# print(feats[feats$instance_id == "adder-12-unsat-shuffled", 1:5])


lt = convertToLlamaCVFolds(ast, add.feature.costs = F)
test1 = do.call(rbind, lt$test)
print(test1[1, 1:5])
lab1 = as.character(test1$best)
print(table(lab1))
# print("llama test")
# print(lt$test[[1]][1, 1:4])
# print("llama train")
# print(lt$train[[1]][1, 1:4])

lres = classify(classifier = J48, data = lt)
mcps1 = unlist(misclassificationPenalties(lt, lres$predictions))
mcp1 = mean(mcps1)
print(summary(mcps1))
print(mcp1)
# foo = function(x) do.call(rbind, x)
# preds1 = as.character(foo(lapply(lres$predictions, foo))$algorithm)
# print(table(preds1))

z = convertToMlr(ast, add.feature.costs = F); mt = z$mlr.task; rin = z$mlr.rin
mi = do.call(c, rin$test.inds)
test2 = z$mlr.task$env$data
test2 = test2[mi, ]
print(test2[1, 1:5])
cost2 = z$mlr.task$env$costs
cost2 = cost2[mi, ]
# lab2 = getMinIndexOfRows(cost2)
lab2 = apply(cost2, 1, which.min)
lab2 = as.character(colnames(cost2)[lab2])
lab2 = str_replace_all(lab2, "xx", "")
print(table(lab2))
# print("mlr test")
# print(mt$env$data[rin$test.inds[[1]][1], 1:3])
# print("mlr train")
# print(mt$env$data[rin$train.inds[[1]][1], 1:3])

lrn = makeCostSensClassifWrapper(makeLearner("classif.J48"))
r = resample(lrn, z$mlr.task, z$mlr.rin, show.info = FALSE)
mcp2 = r$aggr[1]
print(mcp2)
# preds2 = as.character(as.data.frame(r$pred)$response)
# print(table(preds2))

