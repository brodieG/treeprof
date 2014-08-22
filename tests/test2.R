library(plyr)
library(data.table)
library(stringr)
library(functools)
source("R/profiler.R")

fun3 <- function(a, b, c=5L) {
  check_args(
    a=numeric(), 
    b=c_and(c_or(is.integer(b), is.character(b)), is.matrix(b)), 
    c=data.frame()
  )
}

profile(fun3(3, matrix(sample(1:20, 9), nrow=3), data.frame()))
profile(ddply(baseball, ~ year + team, summarize, hr=sum(hr)))

#profile(ddply(baseball, ~ year + team, summarize, hr=sum(hr)))


log.dt[!is.na(get(names(log.dt[2L]))), fun(.SD, .BY, .N), by=eval(as.name(names(log.dt)[1L]))]

log.dt[,,byget(names(log.dt)[1L])]

log.dt[, 
       printAndSplit(.SD, .BY, .N), by = list(get(names(log.dt)[1L]))]


[, 
                                                                  2L:5L, with = FALSE]

fun <- function(x, y, z) browser()
DT <- data.table(a=1:3, b=letters[1:3])
DT[, fun(.SD, .BY, .N), by=eval(names(DT)[2])]

fun2 <- function(dt, by, n) {
  browser(); by
#   if(nrow(dt)) {
#     fun.name <- as.character(by[[1]])
#     depth <<- depth + 1L
#     dt.res <- dt[
#       !is.na(eval(as.name(names(dt)[1L]))), 
#       printAndSplit(.SD, .BY, .N), 
#       by=eval(names(dt)[1L])
#       ][, 2L:5L, with=FALSE]
#     #if(depth == 1) browser()
#     dt.res <- rbind(
#       data.table(
#         fun.name=fun.name, 
#         level=depth, 
#         n=n, 
#         n.self=n - dt.res[level==depth + 1, sum(n)]),
#       dt.res)
#     depth <<- depth - 1L
#   } else {
#     dt.res <- data.table(
#       fun.name=character(0L), 
#       level=integer(0L), 
#       n=integer(0L),
#       n.self=integer(0L)
#     ) }
#   dt.res
}


source("tests/test3.R")
  
runApp("inst/shinyapp")
