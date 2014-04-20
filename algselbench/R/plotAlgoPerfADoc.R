#' EDA plots for performance values of algorithms across all instances.
#'
#' @description
#' If NAs occur, they are imputed (before aggregation) either by
#' \code{base + 0.3 range + jitter} .
#' \code{base} is ithe cutoff value for runtimes tasks with cutoff or
#' the worst performance for all others.
#'
#' Stochastic replications are aggregated by the mean value.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param log [\code{logical(1)}]\cr
#'   Should the performance values be log10-transformed in the plot?
#'   Default is FALSE.
#' @return ggplot2 plot object.
NULL

