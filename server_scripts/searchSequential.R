makeSSControl = function(method = "sfs", alpha = 1, beta = 1, max.bits = Inf, compare = compare.diff) {
  assertChoice(method, c("sfs", "sbs", "sffs", "sfbs"))
  assertNumber(alpha, na.ok = FALSE)
  assertNumber(beta, na.ok = FALSE)
  assertNumber(max.bits, na.ok = FALSE, lower = 0)
  assertFunction(compare)
  makeS3Obj("SSControl", method = method, alpha = alpha, beta = beta,
    max.bits = max.bits, compare = compare)
}

compare.diff = function(oldx, oldy, newx, newy, control, threshold) {
  (oldy - newy) > threshold
}

searchSequential = function(feval, n.bits, control, show.info = TRUE, ...) {
  assertFunction(feval, "xs", ordered = TRUE)
  n.bits = asInt(n.bits, lower = 1, na.ok = FALSE)
  assertClass(control, "SSControl")
  assertFlag(show.info)

  seq.step = function(forward, state, gen.new.states, compare) {
    # we have too many vars already and cannot move forward
    if (forward && !is.na(control$max.bits) && control$max.bits <= sum(unlist(state$x)))
      return(NULL)
    xs = gen.new.states(state$x)
    messagef("Eval new states: %i", length(xs))
    if (length(xs) == 0)
      return(NULL)
    dob = max(opt.path$env$dob) + 1L
    ys = feval(xs, ...)
    Map(function(x, y) {
      addOptPathEl(opt.path, x = list(x = x), y = y, dob = dob, eol = dob)
    }, xs, ys)

    best.i = getOptPathBestIndex(opt.path, dob = dob, ties = "random")
    best = getOptPathEl(opt.path, best.i)
    # best element lives one iteration longer
    thresh = ifelse(forward, control$alpha, control$beta)
    better = compare(state$x, state$y, best$x, best$y, control, thresh)
    messagef("Best new y: %f. Better: %s", best$y, better)
    # if backward step and we have too many vars we do always go to the next best state with one less var.
    if ((forward && better) || (!forward && (better || (!is.na(control$max.bits) && sum(unlist(state$x)) > control$max.bits)))) {
      setOptPathElEOL(opt.path, best.i, dob+1)
      return(best)
    } else {
      return(NULL)
    }
  }

  gen.new.states.sfs = function(x) {
    xs = list()
    for (i in seq_along(x))
      if (x[i] == 0) {
        y = x
        y[i] = 1
        xs[[length(xs)+1L]] = y
      }
    xs
  }

  gen.new.states.sbs = function(x) {
    xs = list()
    for (i in seq_along(x))
      if (x[i] == 1) {
        y = x
        y[i] = 0
        xs[[length(xs)+1L]] = y
      }
    xs
  }

  par.set = makeParamSet(makeIntegerVectorParam("x", len = n.bits))
  opt.path = makeOptPathDF(par.set, y.names = "y", minimize = TRUE)

  compare = compare.diff
  method = control$method

  initx = switch(method,
    sfs = rep(0, n.bits),
    sbs = rep(1, n.bits),
    sffs = rep(0, n.bits),
    sfbs = rep(1, n.bits),
    stop(paste("Unknown method:", method))
  )

  gen.new.states = switch(method,
    sfs = gen.new.states.sfs,
    sbs = gen.new.states.sbs,
    sffs = gen.new.states.sfs,
    sfbs = gen.new.states.sbs,
    stop(paste("Unknown method:", method))
  )
  inity = feval(list(initx), ...)[1L]
  messagef("Init y: %g", inity)
  state = list(x = initx, y = inity)
  addOptPathEl(opt.path, x = list(x = initx), y = inity, dob = 1L, eol = 2L)

  forward = (method %in% c("sfs", "sffs"))
  fail = 0
  while ((method %in% c("sfs", "sbs")  && fail == 0) || (method %in% c("sffs", "sfbs") && fail < 2)) {
    state2 = seq.step(forward, state, gen.new.states, compare)
    # we could not move to state2 in normal step, stay where we are
    if (!is.null(state2)) {
      state = state2
      state$x = unlist(state$x)
      fail = 0
    } else {
      fail = fail + 1
    }
    if (method %in% c("sffs", "sfbs")) {
      #cat("forward:", !forward, "\n")
      gns = switch(method,
        sffs = gen.new.states.sbs,
        sfbs = gen.new.states.sfs
        )
      state2 = seq.step(!forward, state, gns, compare)
      if (!is.null(state2)) {
        state = state2
        fail = 0
      } else {
        fail = fail + 1
      }
    }
  }

  # if last generation contains no better element, go to second to last
  last = max(opt.path$env$dob)

  if (all(opt.path$env$eol[opt.path$env$dob == last] == last))
    last = last-1
  i = getOptPathBestIndex(opt.path, dob = last, ties = "first")
  e = getOptPathEl(opt.path, i)
  list(x = e$x, y = e$y, opt.path = opt.path)
}




