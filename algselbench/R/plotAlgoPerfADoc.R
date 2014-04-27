#' @title EDA plots for performance values of algorithms across all instances.
#'
#' @description
#' If NAs occur, they are imputed (before aggregation) by
#' \code{base + 0.3 range + jitter} .
#' \code{base} is is the cutoff value for runtimes tasks with cutoff or
#' the worst performance for all others.
#'
#' Stochastic replications are aggregated by the mean value.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param impute.zero.vals [\code{logical(1)}]\cr
#'   Should values which are exactly 0 be imputed to 1e-6?
#'   This allows to take the logarithm later on, handy for subsequent visualizations.
#'   Note that this really only makes sense for non-negative measures!
#'   Default is FALSE.
#' @param log [\code{logical(1)}]\cr
#'   Should the performance values be log10-transformed in the plot?
#'   Default is FALSE.
#' @return ggplot2 plot object.
#' @name plotAlgoPerf
#' @rdname plotAlgoPerf
NULL

