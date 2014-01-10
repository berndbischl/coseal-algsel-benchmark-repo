#' Checks the feature data set for duplicated instances.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @return Blocks of duplicated features (if existing).
#' @export
lookForDuplicates = function(astask){
  checkArg(astask, "ASTask")
  data = astask$feature.values
  original.data = data
  nr.of.orig.obs = nrow(original.data)
  data = data[!duplicated(data[,"instance_id"]),]
  label = as.character(data[,"instance_id"])
  data = data[, setdiff(colnames(data), "instance_id")]
  nr.of.unique.obs = nrow(data)
  duplicated.data = duplicated(data)
  nr.of.total.duplicates = sum(duplicated.data)
  if(nr.of.unique.obs != nr.of.orig.obs) 
    catf("%i instances were used, although only %i different instances exist.\n", 
      nr.of.orig.obs, nr.of.unique.obs)
  if(nr.of.total.duplicates == 0L) { 
    if(nr.of.unique.obs == nr.of.orig.obs) {
      cat("Did not recognize any duplicated features.")
    } else {
      cat("(Note: Those duplicates usually result from replications.)")
    }
    return(NULL)
  } else {
    result = NULL
    block = NULL
    blocks = 0L
    max.inst = 0L
    dupl.inst.id = NULL
    existing.nas = any(is.na(data))
    while (any(duplicated.data)) {
      blocks = blocks + 1L
      actual.duplicate = which(duplicated.data)[1L]
      if (existing.nas) {
        x = as.character(data[actual.duplicate, ])
        duplicates = logical(nr.of.unique.obs)
        for (i in 1:nr.of.unique.obs) 
          duplicates[i] = all(as.character(data[i, ]) == x)  
      } else {
        x = data[actual.duplicate, ]
        duplicates = apply(data, 1, function(z) all(z == x))
      }
      block = c(block, rep(blocks, length = sum(duplicates)), "")
      result = c(result, label[duplicates], "")
      nr.of.actual.duplicates = sum(duplicates)
      dupl.inst.id = c(dupl.inst.id, 
        (max.inst + 1L) : (max.inst + nr.of.actual.duplicates), "")
      max.inst = max.inst + nr.of.actual.duplicates
      duplicated.data[duplicates] = FALSE
    }
    if(blocks > 1L){
      catf("The following %i instances result in %i blocks of duplicated features:", 
           blocks + nr.of.total.duplicates, blocks)
    } else {
      catf("The features of the following %i instances are duplicates of each other:", 
           blocks + nr.of.total.duplicates)
    }
    result = cbind(block = block, dupl.inst.id = dupl.inst.id,
      duplicates = result)
    result = result[-nrow(result),]
    result = as.data.frame(result)
    result$block = as.character(result$block)
    result$dupl.inst.id = as.character(result$dupl.inst.id)
    result$duplicates = as.character(result$duplicates)
    colnames(result) = c("Block", "Inst. No.", "ID of duplicated feature")
    return(result)
  }
}