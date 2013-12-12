library(BBmisc)
library(knitr)
library(xtable)
library(ggplot2)
library(devtools)
load_all("~/cos/coseal-algsel-benchmark-repo/algselbench")

data.dir =  "~/cos/coseal/data"
task.dirs = list.files(data.dir, full = TRUE)[c(2)]
rhtml.dir = normalizePath("Rhtml")
html.dir = normalizePath("html")
# url.prefix = "file:///home/bischl/cos/coseal-algsel-benchmark-repo/html"
url.prefix = "http://berndbischl.github.io/coseal-algsel-benchmark-repo/task-pages/"

ee = new.env()
ee$data.dir = data.dir
ee$task.dirs = task.dirs
ee$url.prefix = url.prefix

knit(file.path(rhtml.dir, "index.Rhtml"), output = file.path(html.dir, "index.html"),
  env = ee, quiet = TRUE)

old.wd = getwd()

try({

  for (task.dir in task.dirs) {
    task.name = basename(task.dir)
    messagef("Create pages for: %s", task.name)

    # set task data for knitr
    astask = parseASTask(task.dir)
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
    knitIt("algos")
  }

})


setwd(old.wd)
