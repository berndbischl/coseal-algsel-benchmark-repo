summarizeFeatureRunStatus = function(astask) {
  rs = astask$feature.runstatus
  fns = getFeatureNames(astask)
  n = nrow(rs)
  for (fn in fns)
    rs[, fn] = as.character(rs[, fn])
  print(head(rs))
  # print(fns)
  # tabs = lapply(fns, function(fn) table(rs[, fn]))
  # Filter(tabs, function(x) all(names(x) %in% )
  # print(tabs[[1]])
  inst.presolved = sapply(seq_len(n), function(i) any("presolved" %in% rs[i, fns]))
  messagef("Number of presolved instances: %i", sum(inst.presolved))
  # print(str(rs))

}
