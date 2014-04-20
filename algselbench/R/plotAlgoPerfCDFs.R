#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfCDFs = function(astask, measure, log = FALSE) {
  z = getEDAAlgoPerf(astask, measure, jitter = TRUE, check.log = log,
    format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = z$measure, col = "algorithm")) + stat_ecdf() +
    theme(axis.title.y = element_blank())
  if (log)
    p = p + scale_x_log10()
  p
}



