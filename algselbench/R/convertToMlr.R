#' @rdname convertToLlama
#' @export
convertToMlr = function(astask, measure, feature.steps, add.feature.costs = TRUE) {
  ch = convertToCheck(astask, measure, feature.steps, add.feature.costs)
  desc = astask$desc
  measure = ch$measure; feature.steps = ch$feature.steps

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
  makeCostSensTask(id = desc$task_id, data = feats, costs = as.matrix(costs))
}


