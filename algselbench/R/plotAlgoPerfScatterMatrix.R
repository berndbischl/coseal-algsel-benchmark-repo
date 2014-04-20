#' @rdname plotAlgoPerfADoc
#' @export
plotAlgoPerfScatterMatrix = function(astask, measure, log = FALSE) {
  z = getEDAAlgoPerf(astask, measure, jitter = TRUE, check.log = log, format = "wide")
  d = dropNamed(z$data, "instance_id")
  pairs(d)
}
