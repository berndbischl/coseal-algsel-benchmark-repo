

makeEDAConfig = function(
  algo.perf.boxplots.log = FALSE,
  algo.perf.densities.log = FALSE
) {
  
  makeS3Obj("ASTaskHTMLConfig",
    algo.perf.boxplots.log = algo.perf.boxplots.log,
    algo.perf.densities.log = algo.perf.densities.log
  )
}

print.ASTaskHTMLConfig = function(x, ...) {
  ns = names(x)
  for (i in seq_along(x)) {
   catf("%-30s : %s", ns[i], x[[i]]) 
  }  
}

readEDAConfig = function(path, task.name) {
  conffile = file.path(path, paste(task.name, "R", sep = "."))
  if (file.exists(conffile)) {
    # source config into envir, then construct
    conf = new.env()
    x = try(sys.source(conffile, envir=conf))
    if (is.error(x)) {
      stopf("There was an error in sourcing your configuration file '%s': %s!", 
        conffile, as.character(x))
    }
    return(do.call(makeEDAConfig, as.list(conf)))
  } else {
    # warn, then use default config
    warningf("Config file for task does not exist: %s", task.name)
    return(makeEDAConfig())
  }
}
