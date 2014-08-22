#' Convert a Treeprof object to `data.table`
#' 
#' Primarily used to display the underlying \code{`data.table`} structure of a 
#' \code{`treeprof`} object
#' 
#' @export
<<<<<<< HEAD
<<<<<<< HEAD
#' @aliases as.data.table.treeprof_fun_table
#' @param x a \code{`treeprof`} object
#' @return a \code{`data.table`} object

as.data.table.treeprof <- function(x, keep.rownames=FALSE) {
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
  if(verbose) message(msg)
=======
=======
#' @aliases as.data.table.treeprof_fun_table
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
#' @param x a \code{`treeprof`} object
#' @return a \code{`data.table`} object

as.data.table.treeprof <- function(x, keep.rownames=FALSE) {
  x.cp <- copy(x)
  setattr(x.cp, "class", class(x)[-grep("^treeprof", class(x))])
  x.cp
<<<<<<< HEAD
>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
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
  if(verbose) message(msg)
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
}