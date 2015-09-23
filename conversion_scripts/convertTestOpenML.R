library(reshape2)
library(plyr)
library(BBmisc)

# read data, name cols, and set data types
f = read.table("data_quality.csv", header = FALSE, sep = ",")
colnames(f) = c("dataset", "quality", "val")
a = read.table("evaluations8.csv", header = FALSE, sep = ",",
  stringsAsFactors = FALSE)
colnames(a) = c("task.id", "dataset", "algo", "errmsg", "val")
a$task.id = as.integer(a$task.id)
a$val = as.numeric(a$val)
a$errmsg[grepl(a$errmsg, pattern = "\\\\N")] = NA

messagef("Data qualities: datasets = %i; qualities = %i", 
  length(unique(f$dataset)), length(unique(f$quality)))

tasks.ids = unique(a$task.id)
tasks.n = length(tasks.ids)
algos = unique(a$algo)
algos.n = length(algos)
messagef("Runs: runs = %i, tasks = %i; algos = %i", 
  nrow(a), tasks.n, algos.n)

messagef("Errors: runs = %i, different = %i",
  sum(!is.na(a$errmsg)), length(unique(a$errmsg)) - 1L)

writeLines(unique(a$errmsg), con = "errors.log")

messagef("Errors: err,but val = %i, no err, but na = %i",
  sum(!is.na(a$errmsg) & !is.na(a$val)), sum(is.na(a$errmsg) & is.na(a$val)))

# remove runs where we dont have an output val
a = subset(a, !(is.na(errmsg) & is.na(val)))

mult.runs = ddply(a, "task.id", function(d) {
  subset(d, duplicated(d$algo) | duplicated(d$algo, fromLast = TRUE))  
})
mult.runs = ddply(mult.runs, c("task.id", "algo"), function(d) {
  data.frame(n = nrow(d), diff = max(d$val) - min(d$val))
})

messagef("Many rep runs:")
print(head(sortByCol(mult.runs, "n", asc = FALSE)))
messagef("Diff in rep runs:")
print(head(sortByCol(mult.runs, "diff", asc = FALSE)))

# average repeeated algo runs
a2 = ddply(a, c("dataset", "algo"), function(d) {
  data.frame(val = mean(d$val, na.omit = TRUE))
})

# subset to tasks with many runs, then complete
good.ds = subset(ddply(a2, "dataset", nrow), V1 > 100)$dataset
# good.ds = unique(subset(a, task.id %in% good.tasks)$dataset)
a3 = subset(a2, a2$dataset %in% good.ds)
algo.count = ddply(a3, "algo", function(d) nrow(d) / length(good.ds))
good.algos = subset(algo.count, V1 == 1)$algo
a4 = subset(a3, a3$algo %in% good.algos)
# a4$val[is.na(a4$val)] = 0

f2 = subset(f, dataset %in% good.ds) 
aslib.f = dcast(dataset ~ quality, data = f2, value.var = "val")
aslib.f = droplevels(aslib.f)
aslib.a = dcast(dataset ~ algo, data = a4, value.var = "val")

# impute acc to 0 for failed runs
aslib.a[is.na(aslib.a)] = 0


library(aslib)

# colnames(aslib.f) = make.names(colnames(aslib.f))
colnames(aslib.f) = c("dataset", paste0("f", 1:(ncol(aslib.f)-1)))
#colnames(aslib.a) = c("dataset", paste0("a", 1:(ncol(aslib.a)-1)))

fsteps = setdiff(colnames(aslib.f), "dataset")

desc = makeS3Obj("ASScenarioDesc",
  scenario_id = "openml", 
  features_deterministic = fsteps,
  features_stochastic = character(0),
  algorithms_deterministic = setdiff(colnames(aslib.a), "dataset"),
  algorithms_stochastic = character(0),
 
  performance_measures = "acc",
  performance_type = "quality",
  maximize = TRUE,
 
  algorithm_cutoff_time = NA,
  algorithm_cutoff_memory = NA,
  features_cutoff_time = NA,
  features_cutoff_memory = NA,
  
  number_of_feature_steps = length(fsteps), 
  default_steps = fsteps,
  feature_steps = setNames(lapply(fsteps, function(x) list(provides = x)), fsteps)
)

feature.values = cbind(instance_id = aslib.f$dataset, repetition = 1, aslib.f[, -1])

feature.runstatus = as.matrix(aslib.f[, -1])
feature.runstatus[is.na(feature.runstatus)] = "error"
feature.runstatus[!is.na(feature.runstatus)] = "ok"
feature.runstatus = cbind(instance_id = as.factor(aslib.f$dataset), repetition = 1, 
  as.data.frame(feature.runstatus))

algo.runs = cbind(instance_id = aslib.a$dataset, aslib.a[, -1])
algo.runs = melt(algo.runs, id.vars = "instance_id", value.name = "acc", variable.name = "algorithm")
algo.runs$repetition = 1L
algo.runs$runstatus = ifelse(is.na(algo.runs$acc), "error", "ok")


algo.runstatus = as.matrix(aslib.a[, -1])
algo.runstatus[is.na(algo.runstatus)] = "error"
algo.runstatus[!is.na(algo.runstatus)] = "ok"
algo.runstatus = cbind(instance_id = as.factor(aslib.a$dataset), as.data.frame(algo.runstatus))
algo.runstatus = melt(algo.runstatus, id.vars = "instance_id", value.name = "status")
algo.runstatus$repetition = 1L

cv.splits = data.frame(instance_id = good.ds, repetition = 1L, 
  fold = sample(rep(1:10, length.out = length(good.ds))))

ast = makeS3Obj("ASScenario", 
  desc = desc, 
  feature.runstatus = feature.runstatus, 
  feature.costs = NULL,
  feature.values = feature.values, 
  algo.runs = algo.runs, 
  algo.runstatus = algo.runstatus,
  cv.splits = cv.splits
)

unlink("openml", recursive = TRUE)
writeASScenario(ast, path = "openml")
ast2 = parseASScenario("openml")


library(llama)
lt = convertToLlamaCVFolds(ast2)

#sb = unique(singleBest(lt)$algorithm)
print(mean(misclassificationPenalties(lt, singleBest)))

model = classify(makeLearner("classif.randomForest"), lt)
print(mean(misclassificationPenalties(lt, model)))

model1 = classifyPairs(makeLearner("classif.randomForest"), lt)
print(mean(misclassificationPenalties(lt, model1)))

model2 = regression(makeLearner("regr.randomForest"), lt)
print(mean(misclassificationPenalties(lt, model2)))

model3 = regressionPairs(makeLearner("regr.randomForest"), lt)
print(mean(misclassificationPenalties(lt, model3)))
