library(algselbench)

makeEDAConfig = function(
  algo.perf.impute.nas = TRUE,
  algo.perf.boxplots.log = FALSE,
  algo.perf.scatter.trafo = "identity",
  algo.perf.densities.log = FALSE,
  algo.perf.probabilities.log = TRUE,
  feature.steps.default
) {
  
  makeS3Obj("ASTaskHTMLConfig",
    algo.perf.boxplots.log = algo.perf.boxplots.log,
    algo.perf.densities.log = algo.perf.densities.log,
    algo.perf.probabilities.log = algo.perf.probabilities.log,
    algo.perf.scatter.trafo = algo.perf.scatter.trafo,
    algo.perf.impute.nas = algo.perf.impute.nas,
    feature.steps.default = feature.steps.default
  )
}

print.ASTaskHTMLConfig = function(x, ...) {
  ns = names(x)
  for (i in seq_along(x)) {
   if (ns[i] == "feature.steps.default")
     x[[i]] = paste(x[[i]], collapse = ", ")
   catf("%-30s : %s", ns[i], x[[i]]) 
  }  
}

readEDAConfig = function(conf.path, task.name, data.path) {
  conffile = file.path(conf.path, paste(task.name, "R", sep = "."))
  if (file.exists(conffile)) {
    # source config into envir, then construct
    conf = new.env()
    x = try(sys.source(conffile, envir=conf))
    if (is.error(x)) {
      stopf("There was an error in sourcing your configuration file '%s': %s!", 
            conffile, as.character(x))
    }
    conf.list = as.list(conf)
    default.featsteps = getFeatureStepNames(parseASTask(paste(data.path, task.name, sep = "/")))
    if (!("feature.steps.default" %in% names(conf.list)))
      conf.list$feature.steps.default = default.featsteps
    return(do.call(makeEDAConfig, conf.list))
  } else {
    # warn, then use default config
    warningf("Config file for task does not exist: %s", task.name)
    return(makeEDAConfig())
  }
}
