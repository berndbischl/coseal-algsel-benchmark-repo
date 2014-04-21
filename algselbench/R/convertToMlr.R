#' @rdname convertToLlama
#' @export
convertToMlr = function(astask, measure, feature.steps, add.feature.costs = TRUE) {
  ch = convertToCheck(astask, measure, feature.steps, add.feature.costs)
  desc = astask$desc
  measure = ch$measure; feature.steps = ch$feature.steps

  ### task
  feats = convertFeats(astask, with.instance.id = FALSE)
  cp = convertPerf(astask, measure = measure, feature.steps = feature.steps,
    add.feature.costs = add.feature.costs, with.instance.id = FALSE)
  costs = if (desc$maximize[[measure]])
    -cp$perf
  else
    cp$perf
  # FIXME: bad, we do it as some cns start with numbers, also in this case mlr error msg is wrong: "special chars"
  # this problems also occurs in other plcaces, we need to handle it generally
  cns = colnames(costs)
  cns = paste0("xx", cns)
  colnames(costs) = cns
  mlr.task = makeCostSensTask(id = desc$task_id, data = feats, costs = as.matrix(costs))

  ### CV 
  cv.splits = astask$cv.splits
  folds = max(cv.splits$fold)
  n = length(unique(cv.splits$instance_id))
  rdesc = makeResampleDesc("CV", iters = folds)
  rin = makeResampleInstance(rdesc, size = n)
  all = 1:n
  # these are sorted, like our data
  all.ids = getInstanceNames(astask)
  # get position into sorted ids
  idsToIndices = function(ids) match(ids, all.ids)
  for (i in 1:folds) {
    s = subset(cv.splits, fold == i)
    test.ids = s$instance_id
    train.ids = setdiff(cv.splits$instance_id, test.ids)
    rin$train.inds[[i]] = idsToIndices(train.ids)
    rin$test.inds[[i]] = idsToIndices(test.ids)
  }
  list(mlr.task = mlr.task, mlr.rin = rin)
}


