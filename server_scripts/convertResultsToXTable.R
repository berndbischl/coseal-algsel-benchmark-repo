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
    perf = res[, m$id]
    # control display of numbers
    res.char[, m$id] = sprintf("%5.2f", perf)
    perf.order = order(perf, decreasing = dec)
    # remove vbs, it would always be best...
    perf.order2 = setdiff(perf.order, ind.vbs)
    j.best = perf.order2[1]
    j.best3 = perf.order2[1:3]
    for (g in groups) {
      ind.group = which(res$algo == g)
      j.group = intersect(perf.order2, ind.group)[1]
      colorize(j.group, m$id, col.group)
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
