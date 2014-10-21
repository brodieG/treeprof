#' Per Function Summary
#'
#' Computes execution times for each function, accounting for any repeats, but
#' ignoring contrbutions from nested repeats when summing up total times.
#'
#' @param x treeprof object
#' @return treeprof_fun_table object, which inherits from \code{`data.table`}
#' @export

by_fun <- function(x) {
  if(!inherits(x, "treeprof_norm"))
    stop("Argument `x` must be a `treeprof_norm` object")

  x.cp <- collapse_passthru_funs(copy(x))
  setkey(x.cp, id)

  # For every element, find all the child nodes

  x.all <- x.cp[,
    list(id.child=unlist(get_descendant_nodes(x.cp, id)), fun.name.orig=fun.name),
    by=list(id.orig=id)
  ]
  setkey(x.all, id.child)

  # For each parent-allchildren chain, check to see if any of the children are
  # the same function call (do we need to address Recall? No, we don't since it
  # doesn't have the same name)

  x.cp.all.dup <- x.cp[x.all][
    order(id), by=id.orig,
    list(
      id,
      duplicated=cumsum(fun.name == fun.name.orig) > 1 &
        fun.name == fun.name.orig
    )
  ][, list(duplicated=any(duplicated)), by=id]
  setkey(x.cp.all.dup, id)

  # Add up the times

  x.cp.time <- x.cp.all.dup[x.cp][, by=fun.name,
    list(
      total=as.numeric(sum(ifelse(!duplicated, as.numeric(n.norm), 0))),
      self=sum(as.numeric(n.self.norm)),
      instances=.N,
      instances.dup=sum(duplicated)
  ) ][order(-total)]
  class(x.cp.time) <- c("treeprof_fun_table", class(x.cp.time))
  attr(x.cp.time, "time.unit") <- attr(x, "time.unit")
  x.cp.time
}
#' Compute High Level Summary of \code{`treeprof`}
#'
#' @keywords internal
#' @param x \code{`treeprof_norm`} object
#' @return \code{`treeprof_summary`} object

summarize <- function(x) {
  if(!inherits(x, "treeprof_norm")) stop("Argument `x` must be a treeprof_norm object")
  structure(
    class="treeprof_summary",
    list(
      Ticks=ticks <- x[, sum(n.self)],
      Iterations=attr(x, "meta.data")$iterations,
      `Time Per`=time_format(time_scale(time_per(x))),
      `Time Total`=time_format(time_scale(attr(x, "meta.data")$time))
  ) )
}
