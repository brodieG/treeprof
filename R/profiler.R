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
<<<<<<< HEAD
<<<<<<< HEAD
#' @param verbose logical(1L) whether to output status to screen
=======
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
#' @param verbose logical(1L) whether to output status to screen
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
#' @return a treeprof object, which is really just a `data.table` with some
#'   attributes attached

treeprof <- function(
  expr=NULL, target.time=5, times=NULL, interval=0.001, 
<<<<<<< HEAD
<<<<<<< HEAD
  file=NULL, eval.frame=parent.frame(), gc.torture=FALSE, verbose=TRUE
=======
  file=NULL, eval.frame=parent.frame(), gc.torture=FALSE
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
  file=NULL, eval.frame=parent.frame(), gc.torture=FALSE, verbose=TRUE
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
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

  # Now process resulting Rprof output

<<<<<<< HEAD
<<<<<<< HEAD
  clean_message("Profiling", verbose)
  lines <- run_rprof(                                     # run Rprof and return character vector
    expr.quoted=expr.capt, interval=interval, target.time=target.time, 
    times=times, file=temp.file, frame=eval.frame, gc.torture=gc.torture,
    verbose=verbose)  
  clean_message("Parsing Rprof", verbose)
  prof.mx <- parse_lines(lines$file)                      # cleanup / transform to matrix format 
  res <- melt_prof(prof.mx, lines$meta$levels)            # convert to long format / data.table
=======
  message("Profiling")
=======
  clean_message("Profiling", verbose)
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
  lines <- run_rprof(                                     # run Rprof and return character vector
    expr.quoted=expr.capt, interval=interval, target.time=target.time, 
    times=times, file=temp.file, frame=eval.frame, gc.torture=gc.torture,
    verbose=verbose)  
  clean_message("Parsing Rprof", verbose)
  prof.mx <- parse_lines(lines$file)                      # cleanup / transform to matrix format 
  res <- melt_prof(prof.mx, lines$meta$levels)            # convert to long format / data.table
<<<<<<< HEAD
  message("Almost Done")
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
  
  # Add the meta data

  setattr(res, "meta.data", 
    list(iterations=lines$meta$run.counter, time=lines$meta$time.total)
  )
<<<<<<< HEAD
<<<<<<< HEAD
  clean_message("Done", verbose)
=======
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
  clean_message("Done", verbose)
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
  res
}
#' Actually Run the Code and Capture \code{`Rprof`} Output
#' 
#' @keywords internal
#' @param expr.quoted a quoted expression
#' @param frame a frame to evaluate the quoted expression in
#' @return list containing the file name of the Rprof data plus some meta data

run_rprof <- function(
<<<<<<< HEAD
<<<<<<< HEAD
  expr.quoted, interval, target.time, times, file, frame=parent.frame(), 
  gc.torture, verbose
=======
  expr.quoted, interval, target.time, times, file, frame=parent.frame(), gc.torture
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
  expr.quoted, interval, target.time, times, file, frame=parent.frame(), 
  gc.torture, verbose
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
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
<<<<<<< HEAD
<<<<<<< HEAD
=======
    message("Go One")
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
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
<<<<<<< HEAD
<<<<<<< HEAD
    clean_message("Estimate evaluation time", verbose)

    while(test.run.time < 0.02) {  # If runs too fast, run 50 more times, and so on
      clean_message("Fast Loop, still estimating", verbose)
=======
    message("Time loop 1")

    while(test.run.time < 0.02) {  # If runs too fast, run 50 more times, and so on
      message("Time loop 1 iterate")
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
    clean_message("Estimate evaluation time", verbose)

    while(test.run.time < 0.02) {  # If runs too fast, run 50 more times, and so on
      clean_message("Fast Loop, still estimating", verbose)
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
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
<<<<<<< HEAD
<<<<<<< HEAD
=======
      print(test.run.timed)
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
      if(inherits(attempt, "try-error")) 
        stop("Failed attempting to evaluate argument `expr.quoted`; see previous error.")
      test.run.time <- sum(test.run.timed)
      run.counter <- run.counter + runs
    }
    if((extra.reps <- floor(target.time / test.run.time)) > 2 ) {  # Didn't fill allotted time, so rep more
<<<<<<< HEAD
<<<<<<< HEAD
      clean_message(paste0("Looping to ", target.time, " seconds"), verbose)
=======
      message("Time loop 2")
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
      clean_message(paste0("Looping to ", target.time, " seconds"), verbose)
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
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
<<<<<<< HEAD
<<<<<<< HEAD
=======
      print(test.run.timed)
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
      run.counter <- run.counter + extra.reps * runs
    }    
  }
  levels <- length(sys.calls())
  list(file=file, meta=list(run.counter=run.counter, time.total=sum(test.run.timed), levels=levels))
}
#' Converts \code{`\link{Rprof}`} Output to Matrix
#' 
#' @keywords internal
#' @param a file name
#' @return a matrix containing the parsed and reversed call stacks, with one 
#'   additional NA column added at the end

parse_lines <- function(file) {
<<<<<<< HEAD
<<<<<<< HEAD
=======
  message("Parsing Lines")
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
  con <- file(file)
  lines <- readLines(con)
  close(con)
  lines <- lines[!grepl("^sample\\.interval=[0-9]+$", lines)]
  if(length(lines) < 2L) {
    stop("Log file had fewer than 2 lines")
  }
  if(!all(grepl('^("[^"]*" )+$', lines))) {
    stop(
      "Log file in unexpected format; make sure you are not using function ",
      " names that contain double-quote characters."
  ) }
  tokens <- regmatches(lines[-1L], gregexpr('"([^"]*)"', lines[-1L]))
  log.mx <- matrix(
    NA_character_, nrow=length(tokens), ncol=max(unlist(lapply(tokens, length)))
  )
  for(i in seq_along(tokens)) {
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

melt_prof <- function(mx, levels.to.exclude) {
  if(ncol(mx) < levels.to.exclude + 6L) 
    stop("Logic error, matrix unexpected format; contact maintainer.")
  mx.unique.cols <- apply(mx[, (levels.to.exclude + 1L):(levels.to.exclude + 6L)], 2, unique)
  mx.valid.vals <- c(
    "try", "tryCatch", "tryCatchList", 
    "tryCatchOne", "doTryCatch", "system.time"
  )
  lapply(seq_along(mx.valid.vals),
    function(x) {
      if(! mx.valid.vals[[x]] %in% mx.unique.cols[[x]]) 
        stop("Logic Error, matrix in unexpected format; contact maintainer.")
  } )
  log.dt <- data.table( # workaround due to non implementation of param in data tables so far
    data.frame(mx, stringsAsFactors=FALSE)[-(1L:(levels.to.exclude + 8L))]
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

