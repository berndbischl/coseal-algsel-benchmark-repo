library(knitr)
library(ggplot2)
library(xtable)
# load_all("../algselbench")
library(algselbench)
library(BBmisc)
library(stringr)
source("defs.R")
source("eda_config.R")
source("convertResultsToXTable.R")

data.dir =  normalizePath(file.path(coseal.svn.dir, "data"))
task.dirs = list.files(data.dir, full = TRUE)
# avoid bbob and machine learning for now
task.dirs = task.dirs[!str_detect(task.dirs, "bbob|machine")]
# task.dirs = task.dirs[1]
rhtml.dir = normalizePath("../Rhtml")
html.dir = normalizePath("../html")
config.dir = normalizePath("../configs")

old.wd = getwd()

if (interactive()) {
  url.prefix =  sprintf("file://%s", html.dir)
} else {
  url.prefix = "http://berndbischl.github.io/coseal-algsel-benchmark-repo/task-pages"
}

ee = new.env()
ee$data.dir = data.dir
ee$task.dirs = task.dirs
ee$url.prefix = url.prefix
ee$llama.results = load2("llama_results.RData")
ee$astasks = list()


try({

  for (task.dir in task.dirs) {
    print(task.dir)
    setwd(old.wd)
    task.name = basename(task.dir)
    messagef("Create pages for: %s", task.name)

    # set task data for knitr
    astask = parseASTask(task.dir)
    ee$astasks[[astask$desc$task_id]] = astask
    ee$task.dir = task.dir
    ee$astask = astask

    # create output dir
    out.dir = file.path(html.dir, task.name)
    unlink(out.dir, recursive = TRUE)
    dir.create(out.dir)
    setwd(out.dir)

    # set global options for chunks
    opts_chunk$set(
      echo = FALSE,
      # fig.path = file.path(out.dir, "figure/"),
      fig.width = 10,
      fig.height = 10
    )

    # set global ggplot options
    theme_set(theme_gray(base_size = 18))

    # read EDA HTML config for task
    config = readEDAConfig(astask, config.dir)
    ee$config = config

    # helper to knit rhtml files
    knitIt = function(file, out = file) {
      knit(
        file.path(rhtml.dir, paste(file, "Rhtml", sep = ".")),
        output = file.path(out.dir, paste(out, "html", sep = ".")),
        env = ee, quiet = TRUE
      )
    }

    # create all subpages and copy readme
    knitIt("task_index", "index")
    file.copy(file.path(task.dir, "readme.txt"), out.dir)
    knitIt("data_files")
    knitIt("algos")
    knitIt("features")
    knitIt("llama")
    knitIt("config")
    knitIt("validator")
  }

  knit(file.path(rhtml.dir, "index.Rhtml"), output = file.path(html.dir, "index.html"),
    env = ee, quiet = TRUE)

})


setwd(old.wd)
