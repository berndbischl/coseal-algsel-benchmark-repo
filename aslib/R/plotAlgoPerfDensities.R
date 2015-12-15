#' @rdname plotAlgoPerf
#' @export
plotAlgoPerfDensities = function(asscenario, measure, impute.failed.runs = TRUE,
  impute.zero.vals = FALSE, log = FALSE, rm.censored.runs = TRUE) {
  
  if (rm.censored.runs)
    asscenario = removeCensoredRuns(asscenario)
  z = getEDAAlgoPerf(asscenario, measure, impute.failed.runs = impute.failed.runs, jitter = TRUE,
    impute.zero.vals = impute.zero.vals, check.log = log, format = "long", with.instance.id = FALSE)
  # FIXME: had to remove failed runs manually as na.rm does not work properly
  if (!impute.failed.runs)
    z$data = z$data[!is.na(z$data[,z$measure]), ]
  p = ggplot(z$data, aes_string(x = z$measure, col = "algorithm"))
  # FIXME: I still see warnings here? na.rm in the next line should disable them?
  p = p + geom_density(na.rm = TRUE) + coord_cartesian(xlim = c(z$range[1] + 1e-6, asscenario$desc$algorithm_cutoff_time))
  if (log)
    p = p + scale_x_log10()
  p
}
