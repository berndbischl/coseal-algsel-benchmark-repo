#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfCDFs = function(astask, measure, impute.zero.vals = FALSE, log = FALSE) {
  z = getEDAAlgoPerf(astask, measure, jitter = FALSE, impute.zero.vals = impute.zero.vals, check.log = log,
    format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = z$measure, col = "algorithm")) + stat_ecdf() +
    theme(axis.title.y = element_blank())

  # only plot area where we have successful runs
  ylim = c(0, max(z$success.rate) * 1.1)
  p = p + coord_cartesian(xlim = z$range, ylim = ylim)
  if (log)
    p = p + scale_x_log10()
  p
}



