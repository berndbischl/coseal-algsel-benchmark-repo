#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfBoxplots = function(asscenario, measure, impute.zero.vals = FALSE, log = FALSE,
  impute.failed.runs = TRUE, rm.censored.runs = TRUE) {

  if (rm.censored.runs)
    asscenario = removeCensoredRuns(asscenario)
  z = getEDAAlgoPerf(asscenario, measure, jitter = TRUE, impute.failed.runs = impute.failed.runs,
    impute.zero.vals = impute.zero.vals, check.log = log, format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = "algorithm", y = z$measure, col = "algorithm")) +
    geom_boxplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 1)
    )
  if (log)
    p = p + scale_y_log10()
  p
}

