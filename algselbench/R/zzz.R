#' @import BatchExperiments
#' @import BBmisc
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
