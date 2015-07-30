# import explicitly, because we have a name clash for BBmisc::normalize and llama::normalize
#' @importFrom BBmisc makeS3Obj addClasses sortByCol setColNames dropNamed rangeVal ensureVector which.first convertMatrixType warningf extractSubList seq_row collapse requirePackages stopf catf clipString messagef getMinIndex

#' @import BatchExperiments
#' @import BatchJobs
#' @import checkmate
#' @import corrplot
#' @import ggplot2
#' @import llama
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import plyr
#' @import reshape2
#' @import RWeka
#' @import stringr
#' @import yaml
NULL

.onAttach <-
function(libname, pkgname) {
    parallelRegisterLevels(pkgname, c("tuneLlamaModel"))
}
