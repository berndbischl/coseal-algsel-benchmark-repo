library(algselbench)
library(BBmisc)
source("defs.R")

data.dir =  file.path(coseal.svn.dir, "data")
scenario.dirs = list.files(data.dir, full = TRUE)

createCVSplitFiles = function(scenario.dirs, data.dir, overwrite = FALSE, warn = TRUE) {
  for (scenario.dir in scenario.dirs) {
    scenario.name = basename(scenario.dir)
    messagef("Create CV file for: %s", scenario.name)
    asscenario = parseASScenario(scenario.dir)
    fn = file.path(data.dir, scenario.name, "cv.arff")
    if (file.exists(fn)) {
      if (!overwrite)
        stopf("CV file already exist: %s", fn)
      if (warn)
        warningf("CV file already exist: %s", fn)
    }
    s = createCVSplits(asscenario, folds = 10L, rep = 1L, file = fn)
  }
}

createCVSplitFiles(scenario.dirs, data.dir, overwrite = TRUE, warn = FALSE)
