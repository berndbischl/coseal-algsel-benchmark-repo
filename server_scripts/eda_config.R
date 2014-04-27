library(algselbench)

makeEDAConfig = function(
  algo.perf.impute.zero.vals = FALSE,
  algo.perf.log = FALSE,
  feature.steps.default
) {

  makeS3Obj("ASTaskHTMLConfig",
    algo.perf.impute.zero.vals = algo.perf.impute.zero.vals,
    algo.perf.log = algo.perf.log,
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

# Sources config file and returns a config s3 object
readEDAConfig = function(astask, confpath) {
  checkArg(astask, "ASTask")
  id = astask$desc$task_id
  conffile = file.path(confpath, paste0(id, ".R"))
  if (file.exists(conffile)) {
    # source config into envir, then construct
    conf = new.env()
    x = try(sys.source(conffile, envir = conf))
    if (is.error(x)) {
      stopf("There was an error in sourcing your configuration file '%s': %s!",
        conffile, as.character(x))
    }
    conf = as.list(conf)
    # if we do not have default steps, take all
    if (is.null(conf$feature.steps.default))
      conf$feature.steps.default = getFeatureStepNames(astask)
    return(do.call(makeEDAConfig, conf))
  } else {
    stopf("Config file for task does not exist: %s", id)
  }
}
