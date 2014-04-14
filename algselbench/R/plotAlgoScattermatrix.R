#' Creates a scatterplot matrix of the performance values of the algorithms.
#'
#' @param astask [\code{\link{ASTask}}]\cr
#'   Algorithm selection task.
#' @param measure [\code{character(1)}]\cr
#'   Measure to plot.
#'   Default is first measure in task.
#' @param trafo [\code{function(1)}]\cr
#'   Function applied to the data as preprocessing to the generation of the plot.
#'   Default is identity.
#' @param na.impute [\code{logical(1)}]\cr
#'   Should the values of algorithm runs with non-ok runstatus (missing performance 
#'   values) be imputed? If yes, imputation is done via max + scalar * (max - min) for
#'   minimization problems and via min - scalar * (max - min) for maximization problems.
#'   Default is TRUE.
#' @return  plot object.
#' @export
plotAlgoScattermatrix = function(astask, measure, trafo = identity, na.impute = TRUE){
  checkArg(astask, "ASTask")
  if (missing(measure))
    measure = astask$desc$performance_measures[1]
  else
    checkArg(measure, "character", len = 1L, na.ok = FALSE)
  if (is.character(trafo))
    trafo = get(trafo)
  checkArg(trafo, "function", len = 1L)
  checkArg(na.impute, "logical", len = 1L, na.ok = FALSE)
  
  if (na.impute)
    astask = imputeCrashedRuns(astask)
  algo.perf = astask$algo.runs
  algos = unique(algo.perf$algorithm)
  perf = algo.perf[order(algo.perf[, "instance_id"], algo.perf[, "repetition"],
    algo.perf[, "algorithm"]), measure]
  data = matrix(perf, ncol = length(algos), byrow = TRUE)
  colnames(data) = sort(algos)
  data = apply(data, 2, trafo)
  data = apply(data, 2, function(x) ifelse(is.finite(x), x, NA))
  pairs(data, lower.panel = panel.cor, 
    diag.panel = panel.hist, upper.panel = panel.lm,
    cex.axis = 2)
  trafo.string = extractTrafo(trafo)
  title(sub = bquote(x[shown] == .(trafo.string)))  
}


## helper function that creates a histogram for each algorithm
## on the diagonal of the scatterplot matrix
panel.hist = function(x, ...) {
  usr = par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h = hist(x, plot = FALSE)
  breaks = h$breaks
  nB = length(breaks)
  y = h$counts
  y = y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


## helper function that calculates pearsons correlation coefficient
## for x and y and also colors those correlations depending on their
## value:
## [+0.8, +1.0] green
## [+0.3, +0.8) orange
## (-0.3, +0.3) black
## (-0.8, -0.3] orange
## [-1.0, -0.8] red
panel.cor = function(x, y, ...) {
  usr = par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  index = (!is.na(x) & !is.na(y))
  r = NA
  x = x[index]
  y = y[index]
  if ( (length(unique(x)) > 1) & (length(unique(y)) > 1) )
    r = cor(x, y)  
  colour = ifelse(abs(r) >= 0.8, 
                  ifelse(r > 0, "green", "red"), ifelse(abs(r) < 0.3, "black", "orange"))
  text(0.5, 0.5, sprintf("%.2f", r), cex = 2, col = colour)
}


## helper function that creates a scatterplot of x and y and also adds
## a red regression line to that plot
panel.lm = function(x, y, ...) {
  index = (!is.na(x) & !is.na(y))
  x = x[index]
  y = y[index]
  points(x, y)
  lin.mod = lm(y ~ x)
  if (all(!is.na(lin.mod$coefficients)))
    abline(lin.mod, col = "red", lwd = 2)
}