singleBestBySuccess = function (data = NULL) {
  if (is.null(data)) {
    stop("Need data to determine single best by success!")
  }
  pred = names(sort(colMeans(data$data[,data$success]), decreasing = TRUE)[1])
  pred = unlist(strsplit(pred, "_success"))
  preds = rep.int(pred, length(data$data$best))
  return(lapply(preds, function(l) {
    setNames(data.frame(table(l)), c("algorithm", "score"))
  }))
}

singleBestByPar = function (data = NULL) {
  if (is.null(data)) {
    stop("Need data to determine single best by par!")
  }
  preds = rep.int(names(sort(colMeans(data$data[,data$performance]), decreasing = FALSE)[1]), 
    length(data$data$best))
  return(lapply(preds, function(l) {
    setNames(data.frame(table(l)), c("algorithm", "score"))
  }))
}
