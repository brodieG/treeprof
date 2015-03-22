#' Profile with Rprof and Display As Tree
#'
#' Uses \code{\link{Rprof}} to profile an expression, and processes the data to
#' display as a tree.  Intended for more complex R code for which a visual
#' representation of the call branches is useful.
#'
#' \code{treeprof} returns a \code{"treeprof"} class object.  You may use the
#' \code{\link{shinyfy}} function on that object to invoke an interactive
#' UI to explore the tree, or just rely on the \code{print} method for a
#' terminal display.
#'
#' Since treeprof uses \code{\link{Rprof}} profiling data it is subject to the
#' same limitations as that function.  If you are trying to optimize a small
#' fast function you may be better served by \code{microbenchmark}.
#'
#' @section Repeated Evaluation:
#'
#' \code{Rprof} works by dumping the call stack at specified intervals.  If the
#' code you are profiling consists of many calls, each of which might execute
#' quickly, it is helpful to ensure the code is evaluated sufficient times for
#' each meaningful call to be recorded.  To achieve this \code{treeprof}
#' evaluates the code to profile once and based on that one timing determines
#' how many times to re-evaluate the code to reach approximately five seconds
#' of evaluation time.  We have found five seconds is sufficient to produce
#' useful profiling information without testing our patience too much.
#'
#' You may modify this behavior with the \code{target.time} and \code{times}
#' parameters.
#'
#' @section Timings:
#'
#' \code{treeprof} uses \code{\link{system.time}} to time overall evaluation
#' time.  Since \code{system.time} precision is OS dependent and often limited
#' to milliseconds or worse, we only time the entire process, including all
#' repetitions required to get to our target evaluation time.  As a result, we
#' only provide average evaluation time.
#'
#' The total timing is allocated to each call depending on what fraction of the
#' total recorded ticks each call accounts for.
#'
#' @section Garbage Collection:
#'
#' R's garbage collection can lead to variability in timings, which is
#' particularly problematic with \code{treeprof} since it only reports the
#' average timing.  You can mitigate this variability by forcing a garbage
#' collection prior to each evaluation of the code to profile.  Unfortunately
#' garbage collection carries substantial overhead (on the order of 60ms on
#' our system), so we only enable this behavior on code that takes over 250ms
#' per evaluation.  You can change this behavior with the \code{gc} parameter.
#'
#' Note that \code{target.time} is inclusive of the time required to run the
#' garbage collection, so you may want to consider increasing it if you enable
#' garbage collection for fast evaluating expressions.
#'
#' You may run your code with \code{\link{gctorture}} on, but you will get more
#' stable results by running the same function thousands of times than by using
#' `gctorture` to slow it down by 10-1000x and just running it a couple of
#' times.
#'
#' @export
#' @import data.table
#' @seealso \code{\link{Rprof}}, \code{\link{summaryRprof}}, \code{\link{gc}},
#'   \code{\link{gctorture}}
#' @param expr expression to profile, which is captured by
#'   \code{\link{substitute}}; if you wish to pass a quoted expression to
#'   evaluate use \code{treeprof_}
#' @param interval numeric to dump stack at, NULL to auto-select (see notes)
#' @param target.time numeric number of seconds to run benchmark for; this is an
#'   approximate suggestion and only holds when the expression excutes faster
#'   than the \code{`target.time`}.
#' @param times integer how many times to run the expression; if this is set to
#'   a value other than null parameter \code{`target.time`} is ignored
#' @param eval.fame an environment to evaluate \code{`expr`} in
#' @param gc garbage collection mode: \itemize{
#'   \item "auto" will use \code{gc} before each expression evaluation if the
#'     first expression evaluation lasts more than 250ms, otherwise will not use
#'     \code{gc} except for the first evaluation
#'   \item TRUE runs \code{gc} before each evaluation
#'   \item FALSE never runs \code{gc} (though R may chose to do so on its own)
#'   \item "torture" run with \code{gctorture(TRUE)}
#' }
#' @param verbose logical(1L) whether to output status updates to screen
#' @param collapse.recursion logical(1L) whether to collapse all recursive calls
#'   on tothemselves so they appear as just one sequence of calls instead of
#'   nested sequences; if any calls to \code{Recall} appear these are
#'   automatically stripped from the stack trace so do not use this feature if
#'   you are using a function by that name other than the built-in base function
#' @return a treeprof object, which is really just a \code{\link{data.table}}
#'   with some attributes attached and \code{print/summary} methods

treeprof <- function(
  expr=NULL, target.time=5, times=NULL, interval=0.001,
  file=NULL, eval.frame=parent.frame(), gc="auto", verbose=TRUE,
  collapse.recursion=FALSE
) {
  treeprof_(
    expr.quoted=substitute(expr), target.time=target.time, times=times,
    interval=interval, file=file, eval.frame=eval.frame, gc=gc, verbose=verbose,
    collapse.recursion=collapse.recursion
  )
}
#' @rdname treeprof
#' @export

treeprof_ <- function(
  expr.quoted=NULL, target.time=5, times=NULL, interval=0.001,
  file=NULL, eval.frame=parent.frame(), gc.torture=FALSE, verbose=TRUE,
  collapse.recursion=FALSE
) {
  if(!is.null(interval) && (!is.numeric(interval) || !length(interval) == 1L)) {
    stop("Argument `interval` must be a numeric vector of length 1.")
  }
  if(
    !is.null(times) &&
    (
      !is.numeric(times) || round(times, 0) != times ||
      !length(times) == 1L || times < 1
    )
  ) {
    stop("Argument `times` must be a strictly postive integer vector of length 1.")
  }
  if(
    is.null(times) && (
      !is.numeric(target.time) || !length(target.time) == 1L || target.time < 0.1
    )
  ) {
    stop("Argument `target.time` must be a numeric vector of length 1 greater than 0.1.")
  }
  if(!is.environment(eval.frame))
    stop("Argument `eval.frame` must be an environment.")
  if(is.null(file)) {
    temp.file <- tempfile()
    on.exit(file.remove(temp.file))
  } else {
    temp.file <- file
  }
  if(!length(gc) == 1L) {
    stop("Argument `gc` must be length 1")
  } else if (is.character(gc) && !gc %in% c("auto", "torture")) {
    stop("Argument `gc` must be \"auto\" or \"torture\" or logical")
  } else if (is.logical(gc) && is.na(gc)) {
    stop("Argument `gc` may not be NA")
  } else {
    stop("Argument `gc` must be logical or character (see docs).")
  }
  if(!isTRUE(collapse.recursion) && !identical(collapse.recursion, FALSE))
    stop("Argument `collapse.recursion` must be TRUE or FALSE")

  # Now process resulting Rprof output

  clean_message("Profiling", verbose)
  lines <- run_rprof(                                     # run Rprof and return character vector
    expr.quoted=expr.capt, interval=interval, target.time=target.time,
    times=times, file=temp.file, frame=eval.frame, gc=gc, verbose=verbose
  )
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
  gc, verbose
) {
  # Get Rprof results, may need to run multiple times, and unfortunately as a
  # result of `system.time` inprecision need to jump through hoops if the run
  # time is zero; maybe should switch to `microbenchmark`.  Also, one side
  # effect of this is we don't get times for each loop, but just for the entire \
  # execution

  Rprof(NULL)
  if(identical(gc, "torture")) {
    gc.torture <- TRUE
    gc <- FALSE
  } else gc.torture <- FALSE

  test.run.timed <- treeprof_eval_each(
    expr.quoted, frame, 1L, file, interval, gc = !identical(gc, FALSE),
    gc.torture = gc.torture
  )
  run.counter <- 1L
  clean_message(
    paste0("First run in ", format(test.run.timed), " seconds"), verbose
  )
  if(identical(gc, "auto")) gc <- if(test.run.timed >= 0.25) TRUE else FALSE

  # just repeat requested number of times

  if(!is.null(times)) {
    test.run.timed <- test.run.timed + treeprof_eval_each(
      expr.quoted, frame, times - 1L, file, interval, gc=gc,
      gc.torture=gc.torture
    )
    run.counter <- run.counter + times - 1L
  } else {
    # attempt to run as many times as reqd to get target time.  If evaluation is
    # under precision (roughly) for system.time, keep evaling until we get
    # enough time

    run.multiplier <- 1

    while(test.run.timed < 0.02) {
      clean_message("Fast Loop, still estimating required repetitions", verbose)
      run.multiplier <- run.multiplier * 5
      test.run.timed <- test.run.timed + treeprof_eval_each(
        expr.quoted, frame, times=run.multiplier, file, interval, gc=gc,
        gc.torture=gc.torture
      )
      run.counter <- run.counter + run.multiplier
    }
    times <- ceiling(
      (target.time - test.run.timed) / test.run.timed * run.counter
    )
    # Keep repeating if needed

    if(times > 0) {
      clean_message(paste0("Looping to ", target.time, " seconds"), verbose)
      test.run.timed <- test.run.timed + treeprof_eval_each(
        expr.quoted, frame, times=times, file, interval, gc=gc,
        gc.torture=gc.torture
      )
      run.counter <- run.counter + times
    }
  }
  levels <- length(sys.calls())
  list(
    file=file, meta=list(run.counter=run.counter, time.total=test.run.timed,
    levels=levels)
  )
}
#' Internal Eval Tool
#'
#' Just evals an expression, but with the advantage of having a distinct name
#' that should be popping up for first time in stack.
#'
#' @keywords internal

treeprof_eval_each <- function(
  exp, frame, times, prof.file, interval, gc, gc.torture
) {
  index <- seq(times)
  if(gc.torture) {
    gctorture(gc.torture)
    on.exit(gc.torture=FALSE)
  }
  time <- system.time(
    {
      Rprof(prof.file, interval=interval, append=TRUE)
      for(i in index) treeprof_eval(exp, frame, gc)
      Rprof(NULL)
    },
    gcFirst=FALSE
  )[["elapsed"]]
  if(gc.torture) gctorture(FALSE)
  time
}
treeprof_eval <- function(exp, frame, gc=TRUE) {
  if(gc) gc(FALSE)
  eval(exp, frame)
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

