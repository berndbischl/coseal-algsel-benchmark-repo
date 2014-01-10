#' Creates a table that shows the dominance of one algorithm over another one.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}]. 
#'  Overview of the dominance between the algorithms.
#' @export
findDominatedAlgos = function(astask){
  checkArg(astask, "ASTask")
  data = astask$algo.runs
  splitted.data = split(data, data$algorithm)
  algo.names = names(splitted.data)
  algoNr = length(algo.names)
  result = c(Superior = "none", Inferior = "none")
  for (i in 1:(algoNr - 1)) {
    alg1 = splitted.data[[i]]
    for (j in (i+1):algoNr) {
      alg2 = splitted.data[[j]]
      result = rbind(result, 
        checkDomination(x = alg1, y = alg2, names = algo.names[c(i, j)]))
    }
  }
  result = as.data.frame(result)
  if (nrow(result) == 1) {
    rownames(result) = 1
    return(result)
  }
  ## rm first line, in case there were dominated algorithms:
  result = result[-1, ]
  result = result[order(result[,1]), ]
  rownames(result) = 1:nrow(result)
  return(result)
}


## Helper function that checks whether algorithm x
## is superior / inferior to algorithm y.
## If there's a superior algorithm it returns
## \code{character(2)} where the first element
## is the name of the superior algorithm and the
## second element of the inferior algorithm.
checkDomination = function(x, y, names){
  x$solved = (as.character(x$runstatus) == "ok")
  y$solved = (as.character(y$runstatus) == "ok")
  index = (x$solved | y$solved)
  if(sum(index) == 0) return(NULL)
  reduced.x = x[index,]
  reduced.y = y[index,]
  supAlgo = NULL
  infAlgo = NULL
  result = NULL
  if( all(reduced.x$solved) & 
    (all(reduced.x[reduced.y$solved, "runtime"] <= reduced.y[reduced.y$solved, "runtime"])) )
      return(c(supAlgo = names[1], infAlgo = names[2]))
  if(all(reduced.y$solved) & 
    (all(reduced.y[reduced.x$solved, "runtime"] <= reduced.x[reduced.x$solved, "runtime"])) )
      return(c(supAlgo = names[2], infAlgo = names[1]))
  return(result)
}