#FIXME: handle rep cv
# convert the cv.splits data.frame to an mlr rep.cv resample instance
convertCVSplitsToMlr = function(cv.splits) {
  folds = max(cv.splits$fold)
  n = length(unique(cv.splits$instance_id))
  rdesc = makeResampleDesc("CV", iters = folds)
  rin = makeResampleInstance(rdesc, size = n)
  all = 1:n
  for (i in 1:folds) {
    s = subset(cv.splits, folds == i)
    #FIXME: ids muste be tranformed to inds
    rin$test.inds = s$instance_id
    rin$train.inds = setdiff(all, rin$test.inds)
  }
  return(rin)
}

