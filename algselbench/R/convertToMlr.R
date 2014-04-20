#' @rdname convertToLlama
#' @export
convertToMlr = function(astask, measure, feature.steps, add.feature.costs = TRUE) {
  ch = convertToCheck(astask, measure, feature.steps, add.feature.costs)
  desc = astask$desc
  measure = ch$measure; feature.steps = ch$feature.steps

  feats = convertFeats(astask, with.id = FALSE)
  cp = convertPerf(astask, measure, feature.steps, add.feature.costs)
  costs = if (desc$maximize[[measure]])
    -cp$perf
  else
    cp$perf
  print(summary(costs))
  cns = colnames(costs)
  # FIXME bad, we do it as some cns start with numbers, also in this case mlr error msg is wrong: "special chars"
  cns = paste0("xx", cns)
  colnames(costs) = cns
  makeCostSensTask(id = desc$task_id, data = feats, costs = as.matrix(costs))
}


