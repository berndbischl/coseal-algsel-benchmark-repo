avoidEmptyClasses = function(foldedTask) {
  foldedTask$train = lapply(foldedTask$train, function(data) {
    data$best = as.factor(as.character(data$best))
    return(data)
  })
  return(foldedTask)
}
