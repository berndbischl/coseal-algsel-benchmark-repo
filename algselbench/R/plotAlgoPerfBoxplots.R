#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfBoxplots = function(astask, measure, log = FALSE) {
  z = getEDAAlgoPerf(astask, measure, jitter = TRUE, check.log = log,
    format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = "algorithm", y = z$measure, col = "algorithm")) +
    geom_boxplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 40, vjust = 1)
    )
  if (log)
    p = p + scale_y_log10()
  p
}

