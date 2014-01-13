#' Creates a table that shows the dominance of one algorithm over another one.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure that's been used for analyzing the algorithm performances.
#'   Default is first measure in task.
#' @return [\code{data.frame}]. 
#'  Overview of the dominance between the algorithms.
#' @export
findDominatedAlgos = function(astask, measure){
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  data = astask$algo.runs
  ## convert maximization into minimization
  if (as.vector(astask$desc$maximize[measure]))
    data[, measure] = -1 * data[, measure]
  splitted.data = split(data, data$algorithm)
  algo.names = names(splitted.data)
  nr.of.algos = length(algo.names)
  result = matrix("", nrow = nr.of.algos, ncol = nr.of.algos)
  rownames(result) = algo.names
  colnames(result) = algo.names
  for (i in 1:nr.of.algos) {
    alg1 = splitted.data[[algo.names[i]]]
    for (j in 1:nr.of.algos) {
      if (j == i)
        next
      alg2 = splitted.data[[algo.names[j]]]
      result[i, j] = checkDomination(x = alg1, y = alg2, measure = measure)
    }
  }
  if (all(result == ""))
    return(NULL)
  
  for (i in nr.of.algos:1) {
    if ( all(result[i, ] == "") )
      result = result[-i, ]
    if ( all(result[, i] == "") )
      result = result[, -i]
  }
  result = as.data.frame(result)
  return(result)
}


## Helper function that checks whether algorithm x
## is superior / inferior to algorithm y.
## If there's a superior algorithm it returns
## \code{character(2)} where the first element
## is the name of the superior algorithm and the
## second element of the inferior algorithm.
checkDomination = function(x, y, measure){
  x$solved = (as.character(x$runstatus) == "ok")
  y$solved = (as.character(y$runstatus) == "ok")
  index = (x$solved | y$solved)
  if (sum(index) == 0) 
    return("")
  reduced.x = x[index,]
  reduced.y = y[index,]
  if ( all(reduced.x$solved) ) {
    if ( all(reduced.x[reduced.y$solved, measure] <= reduced.y[reduced.y$solved, measure]) ) {
      if ( any(reduced.x[reduced.y$solved, measure] < reduced.y[reduced.y$solved, measure]) ) {
        return("better")
      }
      return("=")
    }
  }
  if ( all(reduced.y$solved) ) {
    if ( all(reduced.y[reduced.x$solved, measure] <= reduced.x[reduced.x$solved, measure]) ) {
      return("worse")
    }
  }
  return("")
}