#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfDensities = function(astask, measure, impute.zero.vals = FALSE, log = FALSE) {
  z = getEDAAlgoPerf(astask, measure, jitter = TRUE, impute.zero.vals = impute.zero.vals, check.log = log,
    format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = z$measure, col = "algorithm")) + geom_density()
  if (log)
    p = p + scale_x_log10()
  p
}



