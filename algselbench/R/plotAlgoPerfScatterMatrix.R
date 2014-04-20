#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfScatterMatrix = function(astask, measure, log = FALSE) {

  z = getEDAAlgoPerf(astask, measure, jitter = TRUE, check.log = log,
    format = "wide", with.instance.id = FALSE)
  pairs(z$data, log = ifelse(log, "xy", ""))
}
