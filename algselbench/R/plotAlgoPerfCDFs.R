#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfCDFs = function(astask, measure, impute.zero.vals = FALSE, log = FALSE) {
  z = getEDAAlgoPerf(astask, measure, jitter = FALSE, impute.zero.vals = impute.zero.vals, check.log = log,
    format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = z$measure, col = "algorithm")) + stat_ecdf() +
    theme(axis.title.y = element_blank())
  if (log)
    p = p + scale_x_log10()
  p
}



