#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfScatterMatrix = function(astask, measure, impute.zero.vals = FALSE, log = FALSE) {

  z = getEDAAlgoPerf(astask, measure, impute.failed.runs = TRUE, jitter = TRUE,
    impute.zero.vals = impute.zero.vals, check.log = log, format = "wide", with.instance.id = FALSE)
  pairs(z$data, log = ifelse(log, "xy", ""))
}
