#' Convert a Treeprof object to `data.table`
#'
#' Primarily used to display the underlying \code{`data.table`} structure of a
#' \code{`treeprof`} object
#'
#' @export
#' @aliases as.data.table.treeprof_fun_table
#' @param x a \code{`treeprof`} object
#' @return a \code{`data.table`} object

as.dt <- function(x, keep.rownames=FALSE) {
  x.cp <- copy(x)
  setattr(x.cp, "class", class(x)[-grep("^treeprof", class(x))])
  x.cp
}
#' @export

as.data.table.treeprof_fun_table <- function(x, keep.rownames=FALSE) {
  x.cp <- copy(x)
  setattr(x.cp, "class", class(x)[-grep("^treeprof", class(x))])
  x.cp
}

#' Outputs a Message For Overwriting
#'
#' @keywords internal
#' @param msg character
#' @param verbose logical(1L)

clean_message <- function(msg, verbose) {
  if(verbose) cat(msg, "\n", sep="", file=stderr())
}

#' Copies Attributes Lost With `[`
#'
#' Modifies \code{target} by reference.  Only copies a subset of attributes as
#' often it doesn't make sense to copy row names, etc.
#'
#' @keywords internal

copy_attrs <- function(target, current) {
  setattr(target, "meta.data", attr(current, "meta.data"))
  setattr(target, "time.unit", attr(current, "time.unit"))
  setattr(target, "class", class(current))
}
