removeConstantFeatures = function(task) {
  task.data = task$data[, task$features]
  constant.feats = names(which(sapply(task.data, function(x) length(unique(x)) == 1)))
  task$data = task$data[, setdiff(names(task$data), constant.feats)]
  task$features = setdiff(task$features, constant.feats)
  return(task)  
}
