# converts llama results to xtable
printResultsAsHTMLTable = function(res, task.id, col.best = "#FF0000", col.best3 =  "#FFAAAA",
  col.group = "#00FF00") {

  require("xtable")
  res = subset(res, prob == task.id)
  groups = unique(res$algo)

  # display config for measures
  measures = list(
    list(id = "par10", display = "PAR10", minimize = TRUE),
    list(id = "succ", display = "Percentage Solved", minimize = FALSE),
    list(id = "mcp", display = "MCP", minimize = TRUE)
  )

  ind.vbs = which(res[, "model"] == "vbs")
  # we need the table as char matrix, so we can add html tags
  res.char = as.matrix(res)

  # function to color cell(s) in res.char
  colorize = function(inds, meas, col)
    res.char[inds, meas] <<- sprintf("<b><FONT COLOR=\"%s\">%5.2f</FONT></b>", col, res[inds, meas])

  for (m in measures) {
    dec = !m$minimize
    minmax = if (m$minimize) min else max
    perf = res[, m$id]
    # remove vbs, it would always be best...
    perf2 = setdiff(perf, perf[ind.vbs])
    # control display of numbers
    res.char[, m$id] = sprintf("%6.3f", perf)
    # vbs is always best
    j.best = which(perf == minmax(perf2))
    j.best3 = which(perf %in% sort(perf2, dec = dec)[1:3])
    for (g in groups) {
      ind.group = which(res$algo == g & res$model != "vbs")
      k = which(perf[ind.group] == minmax(perf[ind.group]))
      colorize(ind.group[k], m$id, col.group)
    }
    colorize(j.best3, m$id, col.best3)
    colorize(j.best, m$id, col.best)
  }
  # add vbs again and drop some cols
  res.char = dropNamed(res.char, c("id", "prob", "llama.fun", "repl"))
  xt = xtable(res.char, "html")
  print(xt, "html", include.rownames = FALSE, sanitize.text.function = identity)
  invisible(NULL)
}
