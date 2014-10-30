library(algselbench)

makeEDAConfig = function(
  algo.perf.impute.zero.vals = FALSE,
  algo.perf.log = FALSE
) {

  makeS3Obj("ASScenarioHTMLConfig",
    algo.perf.impute.zero.vals = algo.perf.impute.zero.vals,
    algo.perf.log = algo.perf.log
  )
}

print.ASScenarioHTMLConfig = function(x, ...) {
  ns = names(x)
  for (i in seq_along(x)) {
   catf("%-30s : %s", ns[i], x[[i]])
  }
}

# Sources config file and returns a config s3 object
readEDAConfig = function(asscenario, confpath) {
  checkArg(asscenario, "ASScenario")
  id = asscenario$desc$scenario_id
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
    return(do.call(makeEDAConfig, conf))
  } else {
    stopf("Config file for scenario does not exist: %s", id)
  }
}
