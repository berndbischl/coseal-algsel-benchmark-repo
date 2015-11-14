library(aslib)
library(BBmisc)
source("defs.R")

scenario.dirs = list.files(coseal.data.dir, full = TRUE)
scenario.dirs = scenario.dirs[!grepl("README.md", scenario.dirs)]
existing.splits = vapply(scenario.dirs, function(scenario.dir) {
  file.exists(file.path(coseal.data.dir, basename(scenario.dir), "cv.arff"))
}, logical(1L))
scenario.dirs = scenario.dirs[!existing.splits]

createCVSplitFiles = function(scenario.dirs, coseal.data.dir, overwrite = FALSE, warn = TRUE) {
  for (scenario.dir in scenario.dirs) {
    scenario.name = basename(scenario.dir)
    messagef("Create CV file for: %s", scenario.name)
    asscenario = parseASScenario(scenario.dir)
    fn = file.path(coseal.data.dir, scenario.name, "cv.arff")
    if (file.exists(fn)) {
      if (!overwrite)
        stopf("CV file already exist: %s", fn)
      if (warn)
        warningf("CV file already exist: %s", fn)
    }
    s = createCVSplits(asscenario, folds = 10L, rep = 1L, file = fn)
  }
}

createCVSplitFiles(scenario.dirs, coseal.data.dir, overwrite = TRUE, warn = FALSE)
