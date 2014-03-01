library(algselbench)
library(BBmisc)
source("defs.R")

data.dir =  file.path(coseal.svn.dir, "data")
task.dirs = list.files(data.dir, full = TRUE)

createCVSplitFiles = function(task.dirs, data.dir, overwrite = FALSE, warn = TRUE) {
  for (task.dir in task.dirs) {
    task.name = basename(task.dir)
    messagef("Create CV file for: %s", task.name)
    astask = parseASTask(task.dir)
    fn = file.path(data.dir, task.name, "cv.arff")
    if (file.exists(fn)) {
      if (!overwrite)
        stopf("CV file already exist: %s", fn)
      if (warn)
        warningf("CV file already exist: %s", fn)
    }
    s = createCVSplits(astask, folds = 10L, rep = 5L, file = fn)
  }
}

createCVSplitFiles(task.dirs, data.dir, overwrite = TRUE, warn = FALSE)
