# Sources config file and returns a list.
sourceConfig = function(astask) {
  checkArg(astask, "ASTask")
  task.id = astask$desc$task_id
  conf.path = "../configs"
  conffile = file.path(conf.path, paste0(task.id, ".R"))
  if (file.exists(conffile)) {
    # source config into envir, then construct
    conf = new.env()
    x = try(sys.source(conffile, envir=conf))
    if (is.error(x)) {
      stopf("There was an error in sourcing your configuration file '%s': %s!",
        conffile, as.character(x))
    }
    conf = as.list(conf)
    # if we do not have default steps, take all
    if (is.null(conf$feature.steps.default))
      conf$feature.steps.default = getFeatureStepNames(astask)
    return(conf)
  } else {
    # warn, then use default config
    stopf("Config file for task does not exist: %s", task.id)
  }
}
