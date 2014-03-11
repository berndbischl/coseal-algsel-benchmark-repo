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
  convertField("number_of_feature_steps", as.integer)

  desc$maximize = setNames(desc$maximize, desc$performance_measures)
  desc$performance_type = setNames(desc$performance_type, desc$performance_measures)

  # handle groups
  ns = names(desc)
  f.steps = which(str_detect(ns, "^feature_step"))
  feature.steps = list()
  for (i in f.steps) {
    #separate name and feature list section
    s1 = str_split(ns[[i]], " ")[[1]]
    s2 = str_split(desc[[i]], " ")[[1]]
    step.name = str_trim(s1[2])
    # split by comma and trim whitespace
    feats = str_split(s2, ",")[[1]]
    feats = sapply(feats, str_trim, USE.NAMES=FALSE)
    feature.steps[[step.name]] = feats
  }
  desc[f.steps] = NULL
  desc$feature_steps = feature.steps
  addClasses(desc, "ASTaskDesc")
}

