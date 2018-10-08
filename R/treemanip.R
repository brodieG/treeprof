#' Presents A Pruned Version of A Branch
#'
#' Depending on the function, keeps target node (\code{`i`}) children to depth
#' \code{`depth`}, as well as target node parent's, and those parent's direct
#' children (uncles, etc.), or trims all nodes that execute faster than a
#' threshold time.
#'
#' Typically \code{`trim_branch_fast`} is applied after \code{`trim_branch`} as
#' otherwise \code{`trim_branch`} would show the branch all the way to the
#' leaves.
#'
#' @keywords internal
#' @aliases trim_branch_fast
#' @param x data.table
#' @param i the node id
#' @param depth how many levels below \code{`i`} to show
#' @param disp.thresh nodes below which speed threshold to keep (5 mills
#'   default; note: mill = 1/1000 of overall eval time, not millisecond)
#' @return data.table pruned as described

trim_branch <- function(x, i, depth=5L) {
  par.lvl <- x[id == i, level]
  par.ids <- x[id < i & level < par.lvl, tail(id, 1L), by=level][, V1]
  sib.ids <- unlist(
    lapply(par.ids,
      function(par.id) {
        par.lvl <- x[id == par.id, level]
        x[id > par.id & level == par.lvl + 1L & cumsum(id > par.id & level <= par.lvl) == 0, id]
  } ) )
  lvl.curr <- x[id == i, level]
  y <- x[
    id %in% c(par.ids, sib.ids, i) |
    (
      id >= i & level %between% c(lvl.curr, lvl.curr + depth - 1L) &
      cumsum(id > i & level <= lvl.curr) == 0
    )
  ]
  y
}
trim_branch_fast <- function(x, i, disp.thresh) {
  threshold <- x[, max(n) / 1000 * disp.thresh]
  children <- get_descendant_nodes(x, as.integer(i))
  nodes.keep <- if(isTRUE(x[children][id != i, all(n < threshold)])) {
    unlist(children)
  } else {
    i
  }
  x.new <- x[ n >= threshold | id %in% nodes.keep ]
  copy_attrs(x.new, x)
  x.new
}
#' Get \code{`ids`} of Descendant Nodes
#'
#' \code{`get_descendant_nodes`} gets all nodes that have for direct or indirect
#' parent \code{`id.par`}, including \code{`id.par`}, whereas \code{`get_child_nodes`}
#' only returns the direct children (and also the parent).
#'
#' @keywords internal
#' @aliases get_child_nodes
#' @param x a \code{`treeprof`} object
#' @param id.par the id of the node to get the children of
#' @return one column data.table containing requested ids (for joining with)
#'   `x` if desired, note this will also include the parent node

get_descendant_nodes <- function(x, id.par) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a treeprof object")
  if(!is.integer(id.par) || length(id.par) != 1L) stop("Argument `id` must be a 1 length integer")
  if(! id.par %in% x$id) stop("Id supplied (", id.par, ") not in treeprof")
  lvl <- x[id.par == id, level]
  as.dt(x[id >= id.par & (cumsum(level <= lvl & id > id.par) == 0), list(id)])
}
get_child_nodes <- function(x, id.par) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a treeprof object")
  if(!is.integer(id.par) || length(id.par) != 1L) stop("Argument `id` must be a 1 length integer")
  if(! id.par %in% x$id) stop("Id supplied (", id.par, ") not in treeprof")
  lvl <- x[id.par == id, level]
  as.dt(
    x[
      (id >= id.par & (cumsum(level < lvl & id > id.par) == 0) & level == lvl + 1L) |
      id == id.par,
      list(id)
  ] )
}
#' Get \code{`ids`} of Parent Node For Each Node
#'
#' @keywords internal
#' @param x a \code{`treeprof`} object
#' @return integer vector where the first item has the parent id of node id 1, the
#'   second of node id 2, etc.  Nodes that don't exist in the input will have 0L
#'   for parent (root node will too)

get_par_nodes <- function(x) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a treeprof object")
  pars <- integer(max(x$id))
  for(i in seq_along(pars)) {
    lvl <- x[id == i, level]
    pars[[i]] <- if(!length(lvl)) {
      0L
    } else {
      id.match <- x[id < i & level < lvl, tail(id, 1L)]
      if(!length(id.match)) 0L else id.match
  } }
  pars
}

#' Remove All Intermediate Levels of a Passthrough
#'
#' Motivated by functions like \code{`\link{try}`} and \code{`\link{tryCatch}`}
#' that have multiple layers of internal calls but then eventually go to
#' evaluate user code.  The idea here is to collapse all those intermediate
#' levels so they don't clutter the profile tree.
#'
#' Most of these functions seem to have a common pattern: there is the function
#' itself that is called, and then at some point, one final function is called
#' before the user code is evaluated.  We capture this by looking for the initial
#' function, and then within the children of that function, look for the final
#' function subject to the condition that the final function is not followed by
#' a disallowed function.  This last condition is necessary because some functions
#' like \code{`\link{tryCatch}`} call themselves in order to register multiple
#' handlers so we would otherwise think we have exited the function when in
#' reality we haven't.
#'
#' Clearly this isn't completely fool proof, especially if someone writes a
#' function with the same name as some of these base passthru functions.
#'
#' @param x a treeprof object
#' @param passthru a \code{`passthru_fun`} object
#' @return a modified treeprof object

collapse_passthru_fun <- function(x, passthru) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a \"treeprof\" object.")
  if(!inherits(passthru, "passthru_fun")) stop("Argument `passthru` must be a \"passthru_fun\" object.")

  x.cp <- copy(x)

  matches <- x.cp[, id[which(fun.name == passthru$enter)]]
  for(i in matches) {
    # For all the ex.funs, find the first one that isn't followed by a disallowed
    # function

    children <- get_descendant_nodes(x.cp, i)
    ex.fun <- head(
      x.cp[children][,
        id[
          fun.name == passthru$exit &
          c(tail(fun.name, -1L), "") != passthru$dont.exit.if
      ] ],
      1L
    )
    if(!length(ex.fun)) next
    en.lvl <- x.cp[id == i, level]
    ex.lvl <- x.cp[id == ex.fun, level]

    # Assign time of child nodes to first one

    time.total <- x.cp[children][level %between% c(en.lvl, ex.lvl), sum(n.self)]
    x.cp[id == i, n.self:=time.total]

    # Remove all items between entry and exit levels; we are using levels
    # because presumably any functions called between these levels are generared
    # by the function we are trying to ablate.  Note that this creates a seemingly
    # odd pattern where there can be function calls removed both before and
    # after the function calls that are kept

    new.children <- x.cp[children][level <= en.lvl | level > ex.lvl]
    new.children[level > en.lvl, level:=level - ex.lvl + en.lvl]
    setcolorder(new.children, names(x.cp))

    x.cp.new <- rbindlist(list(x.cp[!children], new.children))
    copy_attrs(x.cp.new, x.cp)
    setkey(x.cp.new, id)
    x.cp <- x.cp.new
  }
  x.cp
}
#' Applies All Defined Passthrus
#'
#' @keywords internal
#' @seealso \code{`\link{collapse_passthru_fun}`}, \code{`\link{passthru_fun}`}
#' @param x a \code{`treeprof`} object
#' @param passthrus a list of \code{`passthru_fun`} objects
#' @return a \code{`treeprof`} object

collapse_passthru_funs <- function(x, passthrus=passthru_defined) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a treeprof object")
  if(
    !is.list(passthrus) ||
    !all(vapply(passthrus, inherits, logical(1L), "passthru_fun"))
  ) stop("Argument `passthrus` must be a list of \"passthru_fun\" objects")
  x.new <- copy(x)
  for(i in passthru_defined) x.new <- collapse_passthru_fun(x.new, i)
  x.new
}
#' Represents Pass Through Function We Wish to Condense In Call Stack
#'
#' Prime example is `try`, which generates a whole bunch of calls that are not
#' really useful in trying to figure out what's going on in the profile.
#'
#' These are referred to as pass through functions because they wrap around
#' other code, so we're interested in seeing what the rest of the code is doing
#'
#' @param enter character 1 length the name of the pass through function
#' @param exit character 1 length the name of the function that then evaluates
#'   the rest of the code
#' @param depth integer 1 length the distance between enter and exit in terms of
#'   levels in the call stack; this is necessary to deal with potentially nested
#'   pass through functions; note this includes the entry and exit levels, so if
#'   the entry and exit levels are 1 apart, the depth should be 2, not 1
#' @return a passthru_fun class S3 object

passthru_fun <- function(enter, exit, dont.exit.if) {
  if(!is.character(enter) || length(enter) != 1L)
    stop("Argument `enter` must be a 1 length character vector")
  if(!is.character(exit) || length(exit) != 1L)
    stop("Argument `exit` must be a 1 length character vector")
  if(!is.character(dont.exit.if) || length(dont.exit.if) != 1L)
    stop("Argument `dont.exit.if` must be a 1 length character vector")
  structure(list(enter=enter, exit=exit, dont.exit.if=dont.exit.if), class="passthru_fun")
}
#' List of Passthrus to Collapse
#'
#' @note order matters here (e.g. must remove "try" before "tryCatch")
#'
#' @keywords internal

passthru_defined <- list(
  passthru_fun("try", "doTryCatch", ""),
  passthru_fun("tryCatch", "doTryCatch", "tryCatchList"),
  passthru_fun("withRestarts", "doWithOneRestart", "withRestartList")
)

#' Sort A Treeprof
#'
#' Each branch is sorted in turn recursively.  The treeprof object is re-id'd
#' in the order of the sort.
#'
#' @note set \code{`decreasing=TRUE`} as otherwise the tree stops making sense;
#'   default is set to \code{`FALSE`} for compatibility with generic.
#'
#' @export
#' @param x treeprof object
#' @param decreasing whether to sort descending or not
#' @param treeprof object, "sorted"

sort.treeprof <- function(x, decreasing=FALSE, ...) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a \"treeprof\" object.")
  max.lvl <- x[, max(level)]
  if(max.lvl < 1L) return(x)

  par.nodes <- get_par_nodes(x)
  res.mx <- matrix(integer(), nrow(x), ncol=max.lvl + 1L)
  res <- rep(NA_integer_, nrow(x))

  # For each level, find the parent nodes for all nodes in that level, then
  # roll up to next level and find parent nodes for that level, applying the new
  # parent to the child nodes as well

  for(i in max.lvl:0L) {
    curr.nodes <- x[, ifelse(level == i, id, res)]
    res.mx[, max.lvl - i + 1L] <- curr.nodes
    res <- ifelse(!is.na(curr.nodes), par.nodes[curr.nodes], NA_integer_)
  }
  # Replace all NAs with unique values for grouping; it doesn't really matter
  # what they are so long as they are unique within a column as we will be
  # grouping by them

  res.mx[is.na(res.mx)] <-
    seq(max(res.mx, na.rm=TRUE) + 1L, by=1L, length.out=sum(is.na(res.mx)))

  # Compute total time within each group.  It is okay to have the same times in
  # different groups since sorting is stable so groups with same times shouldn't
  # get mixed in together, though we do need to mix in the original groups so
  # that differences further down the chain don't mix things together

  res.mx.time <- apply(res.mx, 2, function(y) ave(x$n, y, FUN = max))
  res.mx.all <- cbind(res.mx, res.mx.time)[, order(rep(1L:ncol(res.mx), 2L))]
  res.lst <- rev(split(res.mx.all, col(res.mx.all)))
  x.cp <- x[do.call(order, c(res.lst, decreasing=decreasing))]
  x.cp[, id:=1:nrow(x.cp)]
  setkey(x.cp, id)
  copy_attrs(x.cp, x)
  x.cp
}
#' Converts to desired time units
#'
#' Will add two columns to the underlying \code{`data.table`} object with the
#' normalized \code{`n`} and \code{`n.self`} values.  Additionally will attach
#' an attribute to the return object
#'
#' @keywords internal
#' @param treeprof data.table object
#' @param disp.unit what the desired display unit is
#' @return a treeprof_norm object with added normalized time columns

normalize <- function(x, disp.unit) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a treeprof object")
  x.cp <- copy(x)

  # Other prep and cleanup

  ticks.total <- x.cp[, max(n)]                           # total ticks
  time.per <- time_per(x.cp)

  if(disp.unit == "mills") {
    x.cp[,                                                  # ticks in 1000ths
      c("n.norm", "n.self.norm") := list(
        round(n / ticks.total * 1000),
        round(n.self / ticks.total * 1000)
    ) ]
  } else {
    n.scale <- if(disp.unit == "auto") {
      time_scale(x.cp$n / ticks.total * time.per)
    } else {
      time_scale(x.cp$n / ticks.total * time.per, scale=disp.unit)
    }
    disp.unit <- attr(n.scale, "time.unit")
    n.scale.all <- time_format(  # in order to get same formatting, merge into one vector
      time_scale(c(x.cp$n, x.cp$n.self) / ticks.total * time.per, scale=disp.unit), inc.units=FALSE
    )
    x.cp[,
      c("n.norm", "n.self.norm") :=  # now need to split back up
        split(n.scale.all, sort(rep(1:2, length(n.scale.all) / 2)))
    ]
  }
  attr(x.cp, "time.unit") <- disp.unit
  setattr(x.cp, "class", c("treeprof_norm", class(x.cp)))
  x.cp
}
