#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfCDFs = function(asscenario, measure, impute.zero.vals = FALSE, log = FALSE, rm.censored.runs = TRUE) {

  if (rm.censored.runs)
    asscenario = removeCensoredRuns(asscenario)
  z = getEDAAlgoPerf(asscenario, measure, impute.failed.runs = TRUE, jitter = FALSE,
    impute.zero.vals = impute.zero.vals, check.log = log, format = "long", with.instance.id = FALSE)
  p = ggplot(z$data, aes_string(x = z$measure, col = "algorithm")) + stat_ecdf() +
    theme(axis.title.y = element_blank())

  p = p + coord_cartesian(xlim = c(z$range[1] + 1e-6, asscenario$desc$algorithm_cutoff_time), ylim = c(0, 1.1))
  if (log)
    p = p + scale_x_log10()
  p
}


