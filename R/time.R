#' Display Time In Reasonable Units
#'
#' Formats such that \code{`max(min(x), .005 * max(x))`} is at worst one decimal
#' place.  This should work for multi value vectors as well as single value
#' ones.
#'
#' @keywords internal
#' @aliases time_scale
#' @param x numeric to format in seconds
#' @param scale how to compute time units, "auto" or "kilosecond" - "femtosecond"
#' @param signif integer(1L) how many significant digits the largest number should have
#' @param inc.units logical(1L) whether to paste the time units to each number
#' @param trim logical(1L) set to FALSE to keep leading whitespace to align all
#'   numbers
#' @return character formatted numbers for \code{`time_format`}, a
#'   \code{`time_scaled`} S3 object for \code{`time_scale`} containing scaled
#'   times and units

time_format <- function(x, signif=4L, inc.units=TRUE, trim=TRUE) {
  if(!inherits(x, "time_scaled")) stop("Argument `x` must be a \"time_scaled\" object")
  if(!is.numeric(signif) || length(signif) != 1L || signif != round(signif)) {
    stop("Argument `signif` must be a one length integer")
  }
  if(!is.logical(trim) || length(trim) != 1L)
    stop("Argument `trim` must be a one length logical")
  decimals <- max(0, signif - ceiling(log10(max(x))))
  paste0(
    format(
      round(x, decimals), nsmall = decimals, digits=0,
      trim=trim, scientific=FALSE
    ),
    if(inc.units) paste0(" ", attr(x, "time.unit"))
) }
time_scale <- function(x, scale="auto", decimals.max=2L) {
  scales.valid <- c(
    "kiloseconds", "seconds", "milliseconds", "microseconds", "nanoseconds",
    "picoseconds", "femtoseconds"
  )
  if(!is.numeric(x) || any(x < 0)) stop("Argument `x` must be numeric and positive")
  if(
    !is.numeric(decimals.max) || length(decimals.max) != 1L ||
    decimals.max != round(decimals.max) || decimals.max < 0) {
    stop("Argument `decimals.max` must be a one positive integer")
  }
  if(length(scale) != 1L || ! is.character(scale) || ! scale %in% c("auto", scales.valid)) {
    stop("Argument `scale` must be 1 length character and in ",
      'c("auto", "kiloseconds", "seconds", "milliseconds", "microseconds", "nanoseconds", "picoseconds", "femtoseconds")'
  ) }
  x <- x / 1000
  if(decimals.max > 3) decimals.max <- 3
  target.x <- max(min(x), .005 * max(x))
  power <- if(scale == "auto") {
    pow.temp <- max(0, -floor(log10(target.x) / 3))
    if(
      target.x * 10 ^ (pow.temp * 3) > 1000 / 10 ^ decimals.max &
      max(x) * 10 ^ (pow.temp * 3) >= 10000
    ) pow.temp - 1 else pow.temp
  } else {
    match(scale, scales.valid) - 1L
  }
  if(power + 1L > length(scales.valid)) {
    warning("Time too small to scale")
    return(structure(x * 1000, class="time_scaled", time.unit="seconds"))
  }
  time <- x * 10 ^ (power * 3)
  unit <- scales.valid[[power + 1L]]
  structure(time, class="time_scaled", time.unit=unit)
}
#' Retrive the Time Per Iteration of the treeprof
#'
#' @keywords internal
#' @param x a treeprof object

time_per <- function(x) {
  if(!inherits(x, "treeprof")) stop("Argument `x` must be a \"treeprof\" object.")
  iters <- attr(x, "meta.data")$iterations
  attr(x, "meta.data")$time / iters
}
