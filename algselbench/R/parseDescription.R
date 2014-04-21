#' S3 class for ASTaskDesc.
#'
#' Object members
#'
#' \describe{
#' \item{task_id [\code{character(1)}]}{Name of task.}
#' \item{performance_measures [\code{character}]}{Names of measures.}
#' \item{maximize [named \code{character}]}{Maximize measure?}
#' \item{performance_type [named \code{character}]}{Either \dQuote{runtime} or
#'   \dQuote{solution_quality}.}
#' \item{algorithm_cutoff_time [\code{numeric(1)}]}{Cutoff time for an algorithm run.}
#' \item{algorithm_cutoff_memory [\code{numeric(1)}]}{Cutoff memory for an algorithm run.}
#' \item{features_cutoff_time [\code{numeric(1)}]}{Cutoff time for a feature run.}
#' \item{features_cutoff_memory [\code{numeric(1)}]}{Cutoff memory for a feature run.}
#' \item{features_deterministic [\code{character}]}{Names of features that are deterministic.}
#' \item{features_stochastic [\code{character}]}{Names of features that are stochastic.}
#' \item{algorithms_deterministic [\code{character}]}{Names of algorithms that are deterministic.}
#' \item{algorithms_stochastic [\code{character}]}{Names of algorithms that are stochastic.}
#' \item{feature_steps [named \code{list} of \code{character}]}{Names of feature processing steps and
#'   as elements the features they influence.}
#' }
#' @name ASTaskDesc
#' @rdname ASTaskDesc
NULL

# Parses description file and returns an S3 class of the contents
parseDescription = function(path) {
  checkArg(path, "character", len = 1L, na.ok = FALSE)

  # do not warn about EOL
  lines = readLines(file.path(path, "description.txt"), warn = FALSE)
  lines = str_split(lines, ":")
  desc = as.list(str_trim(sapply(lines, function(x) x[2])))
  names(desc) = str_trim(sapply(lines, function(x) x[1]))

  # now handle all non-scalar strings and convert them to proper data types

  convertField = function(name, cast = as.character) {
    val = str_trim(desc[[name]])
    val = if (length(val) == 0L || val == "")
      character(0)
    else if (val == "?")
      NA
    else
      str_trim(str_split(val, ",")[[1]])
    desc[[name]] <<- cast(val)
  }

  convertField("performance_measures")
  convertField("maximize", as.logical)
  convertField("performance_type")
  convertField("algorithm_cutoff_time", as.numeric)
  convertField("algorithm_cutoff_memory", as.numeric)
  convertField("features_cutoff_time", as.numeric)
  convertField("features_cutoff_memory", as.numeric)
  convertField("features_deterministic")
  convertField("features_stochastic")
  convertField("algorithms_deterministic")
  convertField("algorithms_stochastic")

  desc$maximize = setNames(desc$maximize, desc$performance_measures)
  desc$performance_type = setNames(desc$performance_type, desc$performance_measures)

  # handle groups
  ns = names(desc)
  f.steps = which(str_detect(ns, "^feature_step"))
  feature.steps = list()
  for (i in f.steps) {
    #separate name and feature list section
    step.name = str_split(ns[[i]], " ")[[1]][2]
    # split by comma and trim whitespace
    feats = str_split(desc[[i]], ",")[[1]]
    feats = sapply(feats, str_trim, USE.NAMES=FALSE)
    feature.steps[[step.name]] = feats
  }
  desc[f.steps] = NULL
  desc$feature_steps = feature.steps
  addClasses(desc, "ASTaskDesc")
}

