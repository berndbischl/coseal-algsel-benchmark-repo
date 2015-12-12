#' @title EDA plots for performance values of algorithms across all instances.
#'
#' @description
#' If NAs occur, they are imputed (before aggregation) by
#' \code{base + 0.3 range + jitter} .
#' \code{base} is is the cutoff value for runtimes scenarios with cutoff or
#' the worst performance for all others.
#'
#' For the CDFs we only show the visible area where successful runs occurred.
#'
#' Stochastic replications are aggregated by the mean value.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in scenario.
#' @param impute.zero.vals [\code{logical(1)}]\cr
#'   Should values which are exactly 0 be imputed to 1e-6?
#'   This allows to take the logarithm later on, handy for subsequent visualizations.
#'   Note that this really only makes sense for non-negative measures!
#'   Default is FALSE.
#' @param impute.failed.runs [\code{logical(1)}]\cr
#'   Should runtimes for failed runs be imputed?
#'   Default is TRUE.
#' @param rm.censored.runs [\code{logical(1)}]\cr
#'   Should runtimes for censored runs (i.e. runs that have hitted the walltime) be
#'   removed (and eventually be imputed along with the remaining NAs)?
#'   Default is TRUE.
#' @param log [\code{logical(1)}]\cr
#'   Should the performance values be log10-transformed in the plot?
#'   Default is FALSE.
#' @return ggplot2 plot object.
#' @name plotAlgoPerf
#' @rdname plotAlgoPerf
NULL

