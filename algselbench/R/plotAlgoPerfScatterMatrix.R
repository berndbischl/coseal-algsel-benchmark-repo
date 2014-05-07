#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfScatterMatrix = function(asscenario, measure, impute.zero.vals = FALSE, log = FALSE) {

  z = getEDAAlgoPerf(asscenario, measure, impute.failed.runs = TRUE, jitter = TRUE,
    impute.zero.vals = impute.zero.vals, check.log = log, format = "wide", with.instance.id = FALSE)
  pairs(z$data, log = ifelse(log, "xy", ""))
}
