#' @export

summary.treeprof <- function(object, mode="TEXT", ...) {  
  invisible(summary(normalize(object, disp.unit="auto"), mode=mode))
}
#' @export

summary.treeprof_norm <- function(object, mode="TEXT", ...) {
  if(!is.character(mode) || !isTRUE(mode %in% c("TEXT", "HTML")))
    stop("Argument `mode` must be either \"TEXT\" or \"HTML\"")
  cat(x <- as.character(summarize(object), mode=mode))
  invisible(x)
}
#' @export

print.treeprof <- function(x, id.start=1L, depth=10L, disp.thresh=5L, ...) {
  summary(x)
  rendered <- as.character(x, mode="TEXT", id.start, depth)
  cat("", "", rendered, sep="\n")
  invisible(rendered)
}
#' @export

print.treeprof_fun_table <- function(x, ...) {
  cat(as.character(x), sep="\n")
}

#' Transform treeprof Output Into Text Representation
#' 
#' @export
#' @aliases as.character.treeprof_summary, as.character.treeprof_fun_table
#' @param x treeprof object
#' @param mode 1 length character either TEXT or HTML
#' @param id.start int what level to display from
#' @param depth int 1 length how many levels to show
#' @param disp.thresh number of mils below which a branch isn't shown
#' @param disp.unit character(1L) use "mills" to show things really to 1000 total
#'   time, or a time unit (e.g. "seconds", "microseconds"); default will find
#'   time unit that displays most sensibly
#' @return character vector with as many elements as there are lines of output

as.character.treeprof <- function(
  x, mode="TEXT", id.start=1L, depth=10L, disp.thresh=5L,
  disp.unit="auto"
) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a \"treeprof\" object.")
  if(!is.character(mode) || length(mode) != 1L || ! mode %in% c("TEXT", "HTML"))
    stop("Argument `mode` must be either \"TEXT\" or \"HTML\"")
  arg.lst <- list(id.start=id.start, depth=depth, disp.thresh=disp.thresh)
  for(i in names(arg.lst)) {
    arg <- arg.lst[[i]]
    if(!is.numeric(arg) || length(arg) != 1L || round(arg) != arg)
      stop("Argument `", i, "` must be one length integer.")
  }
  if(!is.character(disp.unit) || length(disp.unit) != 1L)
    stop("Argument `disp.unit` must be character(1L).")

  x.new <- copy(x)
  attributes(x.new) <- attributes(x)  # copy strips `treeprof` class so need to restore
  x.new <- collapse_passthru_funs(x.new)
  x.new <- sort(x.new, decreasing=TRUE)
  if(!inherits(x.new, "treeprof_norm")) {
    x.new <- normalize(x.new, disp.unit)  
  }
  if(disp.unit == "auto") disp.unit <- attr(x.new, "time.unit")

  base.level <- x.new[id == id.start, level]
  terminal <- x.new[level >= c(tail(level, -1L), 0L), id]  # terminal node ids, used later
  target.branch <- x.new[id.start == id |                  # the branch we're focused on   
    id > id.start & level >= base.level + 1L & 
    cumsum(id > id.start & level <= base.level) == 0, id
  ]
  x.new <- trim_branch(x.new, id.start, depth=depth)       # levels too far down to show
  x.new <- trim_branch_fast(x.new, id.start, disp.thresh)  # levels too fast to show

  tab.size <- 4L
  max.chars <- max(nchar(x.new$fun.name) + x.new$level * tab.size) + 2L
  max.time.chars <- max(nchar(x.new$n.norm))
  max.time.self.chars <- max(nchar(x.new$n.self.norm))
  disp.unit.row <- paste0(   # shows time units above times
    paste0(
      rep(
        if(mode == "TEXT") " " else "&nbsp;",
        max.chars + max.time.chars + max.time.self.chars + 7L - nchar(disp.unit)
      ),
      collapse=""
    ),
    disp.unit
  )
  # Generate basic print format with offsets for each level, etc.
  
  basic.str <- x.new[,
    (
      function(dt.row) {
        name.chars <- nchar(fun.name)
        paste0(
          vapply(level, function(y) paste0(rep(" ", y * tab.size), collapse=""), character(1L)),
          fun.name, 
          "~",    #marks end of fun.name, will later be removed
          vapply(
            seq_along(name.chars), 
            function(i) {
              paste0(
                rep("-", max.chars - name.chars[[i]] - level[[i]] * tab.size), 
                collapse=""
            ) },
            character(1L)
          )
      ) } )(.SD)
  ]
  # Convert to a 1 character per cell matrix so that we can calcualte
  # where the vertical lines go (connect any pair of calls at the same)
  # level

  str.mx <- do.call(rbind, strsplit(basic.str, split=""))
  str.mx.cols <- ncol(str.mx)
  str.mx.shift <- cbind(" ", str.mx[, -str.mx.cols])
  
  # Now find the previous character so we can look for beginning of string
  # (i.e. " x")
  
  str.mx.2 <- matrix(paste(str.mx.shift, str.mx, sep=""), ncol=str.mx.cols)
  
  cells.to.fill <- apply(
    str.mx.2, 
    2L,
    function(vec) {
      no.space <- grep(" [^ ]", vec)
      no.space.end <- grep("[^ ]{2}", vec)
      if(length(no.space) > 1L) {   # All the lines that both start at same level
        # Each row has the start row end row of two calls at same level (i.e 
        # the gap to bridge with `|` assuming no intervening other text)
        no.space.pairs <- cbind(    
          no.space[-length(no.space)],
          no.space[-1L]
        )
        # But only want those that don't have other intervening lines, which are
        # the ones denoted by no.space.end
        
        no.space.valid <- no.space.pairs[
          apply(
            no.space.pairs,
            1L,
            function(pair, no.space.end) {
              vec.tmp <- no.space.end[no.space.end > min(pair)]
              if(length(vec.tmp[vec.tmp < max(pair)])) {
                FALSE
              } else {
                TRUE
            } },
            no.space.end
          ),
          ,
          drop=FALSE
        ]
        # Now get every row between the line pairs we care about since these
        # are the ones that are getting the vertical connector
        
        if(nrow(no.space.valid) < 1) {
          integer(0)
        } else {
          c(
            unlist(
              apply(no.space.valid, 1, function(vec) setdiff(min(vec):max(vec),vec))
        ) ) } 
      } else {
        integer(0)
  } } )
  # Now create an indexing matrix from the above info; basically, we just
  # need to add the column reference to the above data and make sure we have
  # an n x 2 matrix
  
  if(length(cells.to.fill)) {
    cells.to.fill.index.mx <- do.call(
      rbind,
      mapply(
        function(vec, index) {
          if(length(vec)) {
            cbind(vec, index)
          } else {
            integer(0)
        } },
        cells.to.fill, 
        1:length(cells.to.fill),
        SIMPLIFY=FALSE
    ) )
    str.mx[cells.to.fill.index.mx] <- "|"  
  }
  # add back gap between fun.name and the trailing dashes

  str.vec <- sub(
    "~(-+)$", " \\1",
    apply(str.mx, 1, paste, collapse="")
  )
  str.vec <- paste(
    str.vec,
    x.new[ , 
      paste(
        " : ", 
        format(n.norm, justify="right"), 
        " - ",
        format(n.self.norm, justify="right"), 
        sep=""
    ) ],
    sep=""
  )
  if(identical(mode, "HTML")) {

    # Wow, bloody horrendous way of injecting HTML into our tree structure, but
    # given that the structure is built based on widths of displayed strings,
    # couldn't be calculated if already in HTML.  There must be a more elegant
    # way to add the HTML but can't be f'ed figuring it out.

    # Also, need to handle the span stuff more elegantly.  Was just trying to
    # prototype stuff so didn't bother with css elegance.
    
    lpad <- substr(str.vec, 1, x.new[, level * tab.size])
    rpad <- substr(str.vec, x.new[, level * tab.size] + nchar(x.new[, fun.name]) + 1L, nchar(str.vec))
    meat <- paste0(
      "<span onclick=\"Shiny.onInputChange('navigate', ", x.new$id, ")\">", 
        gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x.new$fun.name))),
      "</span>"
    )
    meat[x.new$id == id.start] <- paste0(
      "<span style='color: blue; font-weight: bold;'>", 
        meat[x.new$id == id.start], 
      "</span>"
    )
    meat[x.new$id %in% terminal] <- paste0(
      "<span style='color: black;'>", 
        meat[x.new$id %in% terminal], 
      "</span>"
    )
    meat[x.new$id != id.start] <- paste0(
      "<span style='color: #3333CC;'>", 
        meat[x.new$id != id.start], 
      "</span>"
    )
    sandwich <- paste0(lpad, meat, rpad)
    target.data <- sandwich[x.new$id %in% target.branch]  # calculated early on
    sandwich[x.new$id %in% target.branch] <- paste0(
      "<span style='opacity: 0.5;'>", 
        substr(target.data, 1L, base.level * tab.size),
      "</span>",
      "<span>",
        substr(target.data, base.level * tab.size + 1L, nchar(target.data)),
      "</span>"
    )
    sandwich[!x.new$id %in% target.branch] <- paste0(
      "<span style='opacity: 0.5;'>",
        sandwich[!x.new$id %in% target.branch],
      "</span>"
    )
    c(
      paste0(
        "<div id='treeproftree' style='font-family: monospace; white-space: pre;'>",
        disp.unit.row
      ),
      sandwich,
      "</div>"
    )
  } else {
    c(disp.unit.row, str.vec)
  }
}
#' @export

as.character.treeprof_summary <- function(x, mode="TEXT") {
  if(!inherits(x, "treeprof_summary")) stop("Argument `x` must be a treeprof_summary object")
  if(!is.character(mode) || !identical(length(mode), 1L) || ! mode %in% c("TEXT", "HTML"))
    stop("Argument `mode` must be a 1 length character vector containing ",
      "\"TEXT\" or \"HTML\"")
  txt <- paste0 (names(x), ": ", unlist(x), collapse="; ")
  if(identical(mode, "HTML")) {
    paste0(
      "<div id=\"treeprofsummary\" style=\"font-family: monospace; white-space: pre; margin: 10px 0px;\">",
      txt,
      "</div>"
    )
  } else {
    txt
  }
}
#' @export

as.character.treeprof_fun_table <- function(x, mode="TEXT", ...) {
  if(!identical(mode, "TEXT")) stop("Argument `mode` must be \"TEXT\"")
  c(
    paste0("Time Units: ", attr(x, "time.unit"), "\n"),
    capture.output(print(as.dt(x)))
  )
}