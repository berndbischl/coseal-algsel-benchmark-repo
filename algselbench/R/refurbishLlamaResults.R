#' Refurbishes the llama experiments data frame by coloring the best values.
#'
#' @param data [\code{data.frame}]\cr
#'   Data frame retrieved from the llama experiments.
#' @param color.best [\code{character(1)}]\cr
#'   HTML-Color used for the three best values within a performance measurement.
#' @param color.best.in.group [\code{character(1)}]\cr
#'   HTML-Color used for highlighting the best performance per group.
#' @return [\code{data.frame}].
#' @export
refurbishLlamaResults = function(data, color.best = "#FF0000", color.best.in.group = "##FFA500") {
  data$model = as.character(data$model)
  for (alg in c("classif", "regr", "cluster")) {
    y = strsplit(data$model[data$algo == alg], split = paste(alg, ".", sep = ""))
    data$model[data$algo == alg] = sapply(y, function(x) x[2])
  }
  vbs.index = which(data$model == "vbs")
  index.per.algo = split(2:nrow(data), data$algo[-vbs.index])
  
  index.best = which(data$best %in% sort(data$best[-vbs.index], decreasing = TRUE)[1:3])
  index.best.in.group = sapply(index.per.algo, function(i) {
    i[data$best[i] %in% max(data$best[i])]})
  index.best.in.group = setdiff(unlist(index.best.in.group), index.best)
  data$best = sprintf("%.4f", data$best)
  if (length(index.best.in.group) > 0)
    data$best[index.best.in.group] = data$best[index.best.in.group] = paste("<b> <FONT COLOR = \"", 
      color.best.in.group, "\"> ", data$best[index.best.in.group], " </FONT> </b>", sep = "")
  data$best[index.best] = paste("<b> <FONT COLOR = \"", color.best, "\"> ", data$best[index.best], 
    " </FONT> </b>", sep = "")
  
  index.mcp = which(data$mcp %in% sort(data$mcp[-vbs.index], decreasing = FALSE)[1:3])
  index.mcp.in.group = sapply(index.per.algo, function(i) {
    i[data$mcp[i] %in% min(data$mcp[i])]})
  index.mcp.in.group = setdiff(unlist(index.mcp.in.group), index.mcp)
  data$mcp = sprintf("%.4f", data$mcp)
  if (length(index.mcp.in.group) > 0)
    data$mcp[index.mcp.in.group] = data$mcp[index.mcp.in.group] = paste("<b> <FONT COLOR = \"", 
      color.best.in.group, "\"> ", data$mcp[index.mcp.in.group], " </FONT> </b>", sep = "")
  data$mcp[index.mcp] = paste("<b> <FONT COLOR = \"", color.best, "\"> ", data$mcp[index.mcp], 
    " </FONT> </b>", sep = "")
  
  index.par = which(data$par %in% sort(data$par[-vbs.index], decreasing = FALSE)[1:3])
  index.par.in.group = sapply(index.per.algo, function(i) {
    i[data$par[i] %in% min(data$par[i])]})
  index.par.in.group = setdiff(unlist(index.par.in.group), index.par)
  data$par = sprintf("%.4f", data$par)
  if (length(index.par.in.group) > 0)
    data$par[index.par.in.group] = data$par[index.par.in.group] = paste("<b> <FONT COLOR = \"", 
      color.best.in.group, "\"> ", data$par[index.par.in.group], " </FONT> </b>", sep = "")
  data$par[index.par] = paste("<b> <FONT COLOR = \"", color.best, "\"> ", data$par[index.par], 
    " </FONT> </b>", sep = "")
 
  data$id = data$prob = data$repl = NULL
  return(data)
}
