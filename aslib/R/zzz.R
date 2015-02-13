#' @import BatchExperiments
#' @import BatchJobs
#' @importFrom BBmisc makeS3Obj addClasses sortByCol setColNames dropNamed rangeVal ensureVector which.first convertMatrixType warningf extractSubList seq_row collapse requirePackages stopf catf clipString messagef getMinIndex
#' @import ParamHelpers
#' @import checkmate
#' @import corrplot
#' @import ggplot2
#' @import llama
#' @import mlr
#' @import parallelMap
#' @import plyr
#' @import reshape2
#' @import RWeka
#' @import stringr
NULL

.onAttach <-
function(libname, pkgname) {
    parallelRegisterLevels(pkgname, c("tuneLlamaModel"))
}
