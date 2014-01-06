#' Creates a table that gives an overview of the performance values per algorithm across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}]. 
#'  Data frame, which gives an overview of the performance values.
#' @export
summarizeAlgoRuns = function(astask) {
  checkArg(astask, "ASTask")
  data = astask$algo.runs
  return(summarizeAlgos(data))
}

summarizeAlgos = function(data) {
  split_all = split(data, data$algorithm)
  split_all = c(split_all, allAlgorithms = list(data))
  var_coeff = function(x) sd(x) / mean(x)
  solved = function(x) 100 * mean(as.character(x) == "ok")
  foo = function(x, aggr) {
    aggr(na.omit(x$runtime))
  }
  result = sapply(split_all, function(z) foo(z, min))
  result = cbind(result, sapply(split_all, function(z) foo(z, function(a) quantile(a, 0.25))))
  result = cbind(result, sapply(split_all, function(z) foo(z, median)))
  result = cbind(result, sapply(split_all, function(z) foo(z, mean)))
  result = cbind(result, sapply(split_all, function(z) foo(z, function(a) quantile(a, 0.75))))
  result = cbind(result, sapply(split_all, function(z) foo(z, max)))
  result = cbind(result, sapply(split_all, function(z) foo(z, sd))) 
  result = cbind(result, sapply(split_all, function(z) foo(z, var_coeff))) 
  result = cbind(result, sapply(split_all, function(z) sum(is.na(z$runtime)))) 
  result = cbind(result, sapply(split_all, nrow))
  result = cbind(result, sapply(split_all, function(z) solved(z$runstatus)))
  colnames(result) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "SD", "VC", "NA's", 
                       "Obs.", "Run OK (%)")
  return(as.data.frame(result))
}


#' Creates a table that gives an overview of the feature values across all instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}]. 
#'  Overview of the feature values.
#' @export
summarizeFeatureValues = function(astask) {
  checkArg(astask, "ASTask")
  featValues = astask$feature.values
  featStatus = astask$feature.runstatus
  return(summarizeFeatures(featValues[,-c(1:2)], featStatus[,-c(1:2, ncol(featStatus))]))
}


summarizeFeatures = function(values, status) {
  var_coeff = function(x) sd(x) / mean(x)
  foo = function(x, aggr) {
    aggr(na.omit(x))
  }
  result = sapply(values, function(z) foo(z, min))
  result = cbind(result, sapply(values, function(z) foo(z, function(a) quantile(a, 0.25))))
  result = cbind(result, sapply(values, function(z) foo(z, median)))
  result = cbind(result, sapply(values, function(z) foo(z, mean)))
  result = cbind(result, sapply(values, function(z) foo(z, function(a) quantile(a, 0.75))))
  result = cbind(result, sapply(values, function(z) foo(z, max)))
  result = cbind(result, sapply(values, function(z) foo(z, sd))) 
  result = cbind(result, sapply(values, function(z) foo(z, var_coeff))) 
  result = cbind(result, sapply(values, function(z) sum(is.na(z)))) 
  result = cbind(result, sapply(values, length))
  result = cbind(result, sapply(status, function(z) 100 * mean(as.character(z) == "ok")))
  colnames(result) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "SD", "VC", "NA's", 
                       "Obs.", "Run OK (%)")
  return(as.data.frame(result))
}

#' Creates a table that shows the dominance of one algorithm over another one.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return [\code{data.frame}]. 
#'  Overview of the dominance between the algorithms.
#' @export
dominatedAlgos = function(astask){
  checkArg(astask, "ASTask")
  data = astask$algo.runs
  splittedData = split(data, data$algorithm)
  algoNames = names(splittedData)
  algoNr = length(algoNames)
  result = c(Superior = "none", Inferior = "none")
  for(i in 1:(algoNr - 1)){
    alg1 = splittedData[[i]]
    for(j in (i+1):algoNr){
      alg2 = splittedData[[j]]
      result = rbind(result, dominating(alg1, alg2, algoNames[c(i, j)]))
    }
  }
  result = as.data.frame(result)
  if(nrow(result) == 1) {
    rownames(result) = 1
    return(result)
  }
  result = result[-1, ]
  result = result[order(result[,1]), ]
  rownames(result) = 1:nrow(result)
  return(result)
}

dominating = function(x, y, names){
  x$solved = (as.character(x$runstatus) == "ok")
  y$solved = (as.character(y$runstatus) == "ok")
  index = (x$solved | y$solved)
  if(sum(index) == 0) return(NULL)
  xRed = x[index,]
  yRed = y[index,]
  supAlgo = NULL
  infAlgo = NULL
  result = NULL
  if(all(xRed$solved) & (all(xRed[yRed$solved, "runtime"] <= yRed[yRed$solved, "runtime"])))
    return(c(supAlgo = names[1], infAlgo = names[2]))
  if(all(yRed$solved) & (all(yRed[xRed$solved, "runtime"] <= xRed[xRed$solved, "runtime"])))
    return(c(supAlgo = names[2], infAlgo = names[1]))
  return(result)
}



#' Checks the feature data set for duplicated instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return Printed output of blocks of duplicated features (if existing).
#' @export
checkDuplicates = function(astask){
  checkArg(astask, "ASTask")
  data = astask$feature.values
  origData = data
  origObsNr = nrow(origData)
  data = data[!duplicated(data$id),]
  label = as.character(data[,"id"])
  data = data[, setdiff(colnames(data), "id")]
  uniqueObsNr = nrow(data)
  dupl = duplicated(data)
  totalDuplicates = sum(dupl)
  if(uniqueObsNr != origObsNr) 
    catf("%i instances were used, although only %i different instances exist\n", origObsNr, uniqueObsNr)
  if(totalDuplicates == 0) { 
    if(uniqueObsNr == origObsNr) {
      cat("Did not recognize any duplicated features.")
    } else {
      cat("\nNo further duplicated features were recognized.")
    }
  } else {
    result = NULL
    blocks = 0
    existingNAs = any(is.na(data))
    while(any(dupl)){
      blocks = blocks + 1
      compareTo = which(dupl)[1]
      if(existingNAs) {
        x = as.character(data[compareTo, ])
        duplicates = logical(uniqueObsNr)
        for(i in 1:uniqueObsNr) duplicates[i] = all(as.character(data[i,]) == x)  
      } else {
        x = data[compareTo, ]
        duplicates = apply(data, 1, function(z) all(z == x))
      }
      block = paste("\n(", blocks, ")", sep = "")
      result = c(result, paste(block, label[duplicates], collapse = ", "))
      dupl[duplicates] = FALSE
    }
    if(blocks > 1){
      catf("The following %i instances result in %i blocks of duplicated features:", 
           blocks + totalDuplicates, blocks)
    } else {
      catf("The features of the following %i instances are duplicates of each other:", 
           blocks + totalDuplicates)
    }
    for(response in result){
      catf("%s", response)
    }
  }
}