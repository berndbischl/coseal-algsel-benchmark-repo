#' Rename the directory names of the output from the validator script.
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
  for (file in files) {
    index = grep(file, output)
    output[index] = sapply(index, function(i) 
      paste(strsplit(output[[i]], paste(task.dir, "/", sep = ""))[[1]], collapse = ""))
  }
  index = which(sapply(output, function(ou) grepl("% Instances: \t\t", ou)))
  output[[index]] = paste(strsplit(output[[index]], "\t\t")[[1]], collapse = "\t\t\t")
  index = which(sapply(output, function(ou) grepl("% Valid Instances: \t", ou)))
  output[[index]] = paste(strsplit(output[[index]], "\t")[[1]], collapse = "\t\t\t")
  index = which(sapply(output, function(ou) grepl("% Presolved: \t\t", ou)))
  output[[index]] = paste(strsplit(output[[index]], "\t\t")[[1]], collapse = "\t\t\t")
  s$output = output
  return(s)
}
