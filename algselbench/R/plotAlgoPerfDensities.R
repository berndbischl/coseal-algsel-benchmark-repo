#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfDensities = function(astask, measure, impute.failed.runs = TRUE, impute.zero.vals = FALSE, log = FALSE) {
  z = getEDAAlgoPerf(astask, measure, impute.failed.runs = impute.failed.runs, jitter = TRUE,
    impute.zero.vals = impute.zero.vals, check.log = log, format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = z$measure, col = "algorithm"))
  # FIXME: It still see warnings here? na.rm n the next line should disable them?
  p = p + geom_density(na.rm = TRUE)
  if (log)
    p = p + scale_x_log10()
  p
}



