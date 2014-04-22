#' Rename the directory names of the output from the validator script.
#'
#' @description
#' Refurbishes the output of the validator script by concealing the path to the
#' files of the coseal directory.
#' Also, assures that the alignment of the output is consistent.
#' 
#' @param s [\code{list(2)}]\cr
#'   List, containing the output of the validator script.
#' @param task.dir [\code{character(2)}]\cr
#'   Path to the algorithm task's directory.
#' @return [\code{list(2)}].\cr
#'   Same list as the input parameter, but without showing the full paths to
#'   the files within the task directory.
#' @export
shortenPathsInValidator = function(s, task.dir) {
  output = s$output
  task.dir = strsplit(task.dir, "/")[[1]]
  task.dir = paste(task.dir[-length(task.dir)], collapse = "/")
  files = c("ground_truth.arff", "citation.bib", "description.txt", 
    "algorithm_runs.arff", "feature_values.arff", "feature_runstatus.arff",
    "feature_costs.arff")
  # only list the local files, but not the entire path
  for (file in files) {
    index = grep(file, output)
    output[index] = sapply(index, function(i) 
      paste(strsplit(output[[i]], paste(task.dir, "/", sep = ""))[[1]], collapse = ""))
  }
  output = alignOutput(output, "Instances", "\t\t")
  output = alignOutput(output, "Valid Instances", "\t")
  output = alignOutput(output, "Presolved", "\t\t")
  s$output = output
  return(s)
}


# help function, assuring that the validator output for certain strings is properly aligned 
alignOutput = function(output, string, sep) {
  index = which(sapply(output, function(ou) grepl(sprintf("%% %s: %s", string, sep), ou)))
  if (length(index) > 0)
    output[[index]] = paste(strsplit(output[[index]], sep)[[1]], collapse = "\t\t\t")
  return(output)
}
