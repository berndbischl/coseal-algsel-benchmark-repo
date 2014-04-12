#' Converts llama results to xtable 
#'
#' @param data [\code{data.frame}]\cr
#'   Data frame retrieved from the llama experiments.
#' @param color.best [\code{character(1)}]\cr
#'   HTML-Color used for the best performance.
#' @param color.best3 [\code{character(1)}]\cr
#'   HTML-Color used for the three best values within a performance.
#' @param color.best.in.group [\code{character(1)}]\cr
#'   HTML-Color used for highlighting the best performance per group.
#' @return [\code{data.frame}].
#' @export
convertResultsToXTable = function(res, task.id, col.best = "#FF0000", col.best3 =  "#FF0000", 
  col.group = "##FFA500") {

  groups = setdiff(unique(data$algo), "baseline")
  
  # display config for measures
  measures = list(
    list(id = "par10", display = "PAR10", minimize = TRUE),
    list(id = "succ", display = "Percentage Solved", minimize = FALSE),
    list(id = "mcp", display = "MCP", minimize = TRUE)
  )
    
  # remove vbs for colorization
  ind.vbs = which(data[, "model"] == "vbs")
  data.vbs = data[ind.vbs, ]
  data = data[-ind.vbs, ]
  # we need the table as char matrix, so we can add html tags
  data.char = as.matrix(data)

  # index.per.algo = split(2:nrow(data), data$algo[-vbs.index])
  
  # function to color cell(s) in data.char
  colorize = function(inds, meas, col)
    data.char[inds, meas] <<- sprintf("<b><FONT COLOR='%s'>%s</FONT></b>", col, data.char[inds, meas])

  for (m in measures) {
    dec = !m$minimize
    perf = data[, m$id]
    perf.order = order(perf, decreasing = dec)
    j.best = perf.order[1]
    j.best3 = perf.order[1:3]
    colorize(j.best, m$id, col.best)    
    colorize(j.best3, m$id, col.best3)   
    for (g in groups) {
      j.group = order(perf[data$algo == g], decreasing = dec)[1]
      colorize(j.group, m$id, col.group)   
    }
  }
  # add vbs again and drop some cols
  data.char = rbind(data.vbs, data.char)
  data.char = dropNamed(data.char, c("id", "prob", "llama.fun", "repl"))
  return(data.char)
}
