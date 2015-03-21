#' Profile with Rprof and Display As Tree
#'
#' Uses RProf to profile an expression which will be repeatedly evaluated until
#' target execution time is met.
#'
#' @note timings are approximate as there is some small overhead to manage the
#'   repeated tests
#'
#' @export
#' @import data.table
#' @param expr expression to profile
#' @param interval numeric to dump stack at, NULL to auto-select (see notes)
#' @param target.time numeric number of seconds to run benchmark for; this is an
#'   approximate suggestion and only holds when the expression excutes faster
#'   than the \code{`target.time`}.
#' @param times integer how many times to run the expression; if this is set to
#'   a value other than null parameter \code{`target.time`} is ignored
#' @param eval.fame an environment to evaluate \code{`expr`} in
#' @param gc.torture whether to enable \code{`\link{gctorture}`}, note that this
#'   should not be necessary since \code{`treeprof`} automatically re-runs a
#'   function enough times to get reasonable resolution; you will get more
#'   stable results by running the same function thousands of times than by
#'   using `gctorture` to slow it down by 10-1000x and just running it a couple
#'   of times.
#' @param verbose logical(1L) whether to output status to screen
#' @param collapse.recursion whether to collapse all recursive calls on to
#'   themselves so they appear as just one sequence of calls instead of nested
#'   sequences; if any calls to \code{Recall} appear these are automatically
#'   stripped from the stack trace so do not use this feature if you are using
#'   a function by that name other than the built-in base function
#' @return a treeprof object, which is really just a `data.table` with some
#'   attributes attached

treeprof <- function(
  expr=NULL, target.time=5, times=NULL, interval=0.001,
  file=NULL, eval.frame=parent.frame(), gc.torture=FALSE, verbose=TRUE,
  collapse.recursion=FALSE
) {
  expr.capt <- substitute(expr)
  if(!is.null(interval) && (!is.numeric(interval) || !length(interval) == 1L)) {
    stop("Argument `interval` must be a numeric vector of length 1.")
  }
  if(!is.null(times) && (!is.numeric(times) || round(times, 0) != times|| !length(times) == 1L)) {
    stop("Argument `times` must be an integer vector of length 1.")
  }
  if(is.null(times) && (!is.numeric(target.time) || !length(target.time) == 1L)) {
    stop("Argument `target.time` must be a numeric vector of length 1.")
  }
  if(!is.environment(eval.frame))
    stop("Argument `eval.frame` must be an environment.")
  if(is.null(file)) {
    temp.file <- tempfile()
    on.exit(file.remove(temp.file))
  } else {
    temp.file <- file
  }
  if(!is.logical(gc.torture) || length(gc.torture) != 1L)
    stop("Argument `gctorture` must be a one length integer")
  if(!isTRUE(collapse.recursion) && !identical(collapse.recursion, FALSE))
    stop("Argument `collapse.recursion` must be TRUE or FALSE")

  # Now process resulting Rprof output

  clean_message("Profiling", verbose)
  lines <- run_rprof(                                     # run Rprof and return character vector
    expr.quoted=expr.capt, interval=interval, target.time=target.time,
    times=times, file=temp.file, frame=eval.frame, gc.torture=gc.torture,
    verbose=verbose)
  clean_message("Parsing Rprof", verbose)
  prof.mx <- parse_lines(lines, collapse.recursion)       # cleanup / transform to matrix format
  res <- melt_prof(prof.mx)                               # convert to long format / data.table

  # Add the meta data

  setattr(res, "meta.data",
    list(iterations=lines$meta$run.counter, time=lines$meta$time.total)
  )
  clean_message("Done", verbose)
  res
}
#' Actually Run the Code and Capture \code{`Rprof`} Output
#'
#' @keywords internal
#' @param expr.quoted a quoted expression
#' @param frame a frame to evaluate the quoted expression in
#' @return list containing the file name of the Rprof data plus some meta data

run_rprof <- function(
  expr.quoted, interval, target.time, times, file, frame=parent.frame(),
  gc.torture, verbose
) {
  # Get Rprof results, may need to run multiple times, and unfortunately as a
  # result of `system.time` inprecision need to jump through hoops if the run
  # time is zero; maybe should switch to `microbenchmark`.  Also, one side
  # effect of this is we don't get times for each loop, but just for the entire \
  # execution
  on.exit(gctorture(FALSE))
  if(!is.null(times)) { # just repeat requested number of times
    Rprof(NULL)
    gc(FALSE)
    gctorture(gc.torture)
    Rprof(file, interval=interval)
    attempt <- try(
      test.run.timed[[1L]] <- system.time(
      for(i in seq(times)) {
        eval(expr.quoted, frame)
      }, gcFirst=FALSE)[["elapsed"]]
    )
    Rprof(NULL)
    gctorture(FALSE)
    if(inherits(attempt, "try-error"))
      stop("Failed attempting to evaluate argument `expr.quoted`; see previous error.")
    run.counter <- times
  } else {             # attempt to run as many times as reqd to get target time
    mult <- 1
    Rprof(NULL)
    test.run.timed <- numeric(1L)
    gc(FALSE)
    gctorture(gc.torture)
    Rprof(file, interval=interval)
    attempt <- try(
      test.run.timed[[1]] <- system.time(eval(expr.quoted, frame), gcFirst=FALSE)[["elapsed"]]
    )
    Rprof(NULL)
    gctorture(FALSE)
    if(inherits(attempt, "try-error")) {
      stop("Failed attempting to evaluate argument `expr.quoted`; see previous error.")
    }
    test.run.time <- test.run.timed[[1]]
    run.multiplier <- 50
    runs <- run.counter <- 1
    clean_message("Estimate evaluation time", verbose)

    while(test.run.time < 0.02) {  # If runs too fast, run 50 more times, and so on
      clean_message("Fast Loop, still estimating", verbose)
      gctorture(gc.torture)
      Rprof(file, interval=interval, append=TRUE)
      attempt <- try(
        test.run.timed[[length(test.run.timed) + 1L]] <- system.time(  # Growing vector, but doesn't happen much
          for(i in 1:(runs <- runs * run.multiplier)) {
            eval(expr.quoted, frame)
          },
          gcFirst=FALSE
        )[["elapsed"]]
      )
      Rprof(NULL)
      gctorture(FALSE)
      if(inherits(attempt, "try-error"))
        stop("Failed attempting to evaluate argument `expr.quoted`; see previous error.")
      test.run.time <- sum(test.run.timed)
      run.counter <- run.counter + runs
    }
    if((extra.reps <- floor(target.time / test.run.time)) > 2 ) {  # Didn't fill allotted time, so rep more
      clean_message(paste0("Looping to ", target.time, " seconds"), verbose)
      gctorture(gc.torture)
      Rprof(file, interval=interval, append=TRUE)
      attempt <- try(
        test.run.timed[[length(test.run.timed) + 1L]] <- system.time(
        for(i in 1:(extra.reps * run.counter)) {
          eval(expr.quoted, frame)
        }, gcFirst=FALSE)[["elapsed"]]
      )
      Rprof(NULL)
      gctorture(FALSE)
      if(inherits(attempt, "try-error"))
        stop("Failed attempting to evaluate argument `expr.quoted`; see previous error.")
      print(test.run.timed)
      run.counter <- run.counter + extra.reps * runs
    }
  }
  levels <- length(sys.calls())
  list(file=file, meta=list(run.counter=run.counter, time.total=sum(test.run.timed), levels=levels))
}
#' Converts \code{`\link{Rprof}`} Output to Matrix
#'
#' @keywords internal
#' @param collapse.recursion logical(1L) whether to collapse recursive calls
#'   on themselves
#' @param lines a list with file name and file meta data as produced by
#'   \code{\link{run_rprof}}
#' @return a matrix containing the parsed and reversed call stacks, with one
#'   additional NA column added at the end

parse_lines <- function(lines, collapse.recursion=FALSE) {
  con <- file(lines$file)
  lines.text <- readLines(con)
  close(con)
  lines.text <- lines.text[!grepl("^sample\\.interval=[0-9]+$", lines.text)]  # expensive check
  if(length(lines.text) < 2L) {
    stop("Log file had fewer than 2 lines")
  }
  if(!all(grepl('^("[^"]*" )+$', lines.text))) {
    stop(
      "Log file in unexpected format; make sure you are not using function ",
      " names that contain double-quote characters."
  ) }
  # Get rid of baseline calls that are unrelated to what we're profiling

  lines.trim <- gsub(
    paste0('("[^"]*" ){0,', lines$meta$levels + 8L, '}$'),
    "", lines.text
  )
  # Collapse recursion

  if(collapse.recursion) {
    lines.trim <- gsub("\"Recall\" ", "", lines.trim)       # Recall just calls function, so blow them away
    pat <- "(((?:\"[^\"]+\" )+?)\\2+)"
    lines.trim <- gsub(pat, "\\2", lines.trim, perl=TRUE)
  }
  tokens <- regmatches(lines.trim[-1L], gregexpr('"([^"]*)"', lines.trim[-1L]))
  log.mx <- matrix(
    NA_character_, nrow=length(tokens), ncol=max(unlist(lapply(tokens, length)))
  )
  for(i in seq_along(tokens)) {
    if(length(tokens[[i]]))
      log.mx[i, 1L:length(tokens[[i]])] <- rev(tokens[[i]])
  }
  # Remove quotes, should only be at beginning and end of each element, pluss
  # add parens

  cbind(gsub("\"", "", log.mx, fixed=TRUE), NA_character_)
}

#' Generate The Final Treeprof Object
#'
#' @keywords internal
#' @param mx a character matrix where each row represents a tick dump, and each
#'   column a level in the stack
#' @return a \code{`treeprof`} object

melt_prof <- function(mx) {
  log.dt <- data.table( # workaround due to non implementation of param in data tables so far
    data.frame(mx, stringsAsFactors=FALSE)
  )
  setnames(log.dt, paste0("V", 1:ncol(log.dt)))
  res <- log.dt[
    !is.na(eval(as.name(names(log.dt)[2L]))),
    chop_stack_recurse(.SD, .BY, .N),
    by=eval(names(log.dt)[1L])
  ][, 2L:5L, with=FALSE]
  setattr(res, "class",  c("treeprof", class(res)))
  res[, id:=1:nrow(res)]
  setkey(res, id)
  res
}
#' Recursively Descend into Rprof Ouput to Create Data for \code{`treeprof`}
#'
#' Key next step is to prove that the output datatable can meaningfully
#' be interpreted by row order.  In other words, if level n shows up once,
#' and then at some later point you get level n again, then you are guaran-
#' teed that it is a from a different call stack.  Similarly, for all levels
#' n, all subsequent rows with levels > n are children to that row.
#'
#' Prelim, it seems this is true since every time data.table groups it
#' effectively orders? Actually, not so, consider the sequence:
#'
#' a b c
#' a e
#' a b d
#'
#' These would erroneously get collapsed into the same stack at b.  Is that bad?
#' This is fine, but needs to be documented to avoid confusion
#'
#' Also, other items:
#' - remember to check whether a function is called from different call stacks
#'   to aggregate how much time is taken up by that function
#'
#' @keywords internal
#' @param dt a data table
#' @param by a one length character vector denoting what the grouping variable
#'   value is
#' @param n how many items in each group
#' @param depth how many levels we have recursed
#' @return data table

chop_stack_recurse <- function(dt, by, n, depth=0L) {
  if(nrow(dt)) {
    fun.name <- as.character(by[[1]])
    dt.res <- dt[
      !is.na(eval(as.name(names(dt)[1L]))),
      chop_stack_recurse(.SD, .BY, .N, depth + 1L),
      by=eval(names(dt)[1L])
    ][, 2L:5L, with=FALSE]
    dt.res <- rbind(
      data.table(
        fun.name=fun.name,
        level=depth,
        n=n,
        n.self=n - dt.res[level==depth + 1, sum(n)]),
      dt.res)
  } else {
    dt.res <- data.table(
      fun.name=character(0L),
      level=integer(0L),
      n=integer(0L),
      n.self=integer(0L)
  ) }
  dt.res
}

