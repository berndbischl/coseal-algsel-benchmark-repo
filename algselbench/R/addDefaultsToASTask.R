#' Integrates default settings from the configs file to the description object.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Description object for the algorithm selection task.
#' @param conf.path [\code{character(1)}]\cr
#'   Path to configs directory of benchmark data set.
#' @return [\code{\link{ASTask}}]. Description object.
#' @export
addDefaultsToASTask = function(astask, conf.path) {
  checkArg(astask, "ASTask")
  checkArg(conf.path, "character", len = 1L, na.ok = FALSE)
  astask$desc$default_feature_steps = getFeatureStepNames(astask)
  conffile = file.path(conf.path, paste(tolower(astask$desc$task_id), "R", sep = "."))
  if (file.exists(conffile)) {
    # source config into envir, then construct
    conf = new.env()
    x = try(sys.source(conffile, envir=conf))
    if (is.error(x)) {
      stopf("There was an error in sourcing your configuration file '%s': %s!", 
            conffile, as.character(x))
    }
    conf.list = as.list(conf)
    if ("feature.steps.default" %in% names(conf.list))
      astask$desc$default_feature_steps = conf.list$feature.steps.default
  } else {
    # warn, then use default config
    warningf("Config file for task does not exist: %s", astask$desc$task_id)
  }
  return(astask)
}
