library("data.table")
library("stringr")
library("plyr")
library(stackbench)

profile(ddply(baseball, ~ year + team, summarize, hr=sum(hr)))
x <- treeprof(fun3(3, matrix(sample(1:20, 9), nrow=3), data.frame()))
capture.output(print(x, mode="HTML"), file="../../test.html")

my_fun <- function(x) sample(x * 1e3)
profile({
    df <- data.frame(a=1:2600, b=letters)
    df[1, 1] <- "hello"

    })
)




prof.mx <- parse_lines(lines$raw)                       # cleanup / transform to matrix format
res <- melt_prof(prof.mx)                               # convert to long format / data.table


x <- treeprof(fun(alb=matrix(1:100, nrow=10), bat=5, cour=b), target.time=1)  # for faster iteration in testing
x <- treeprof(fun(alb=matrix(1:100, nrow=10), bat=5, cour=b), target.time=5)




capture.output(print(x, mode="HTML"), file="../../test.html")
fun2 <- function(albatross, batman, courser) {
  match_call()
}
microbenchmark(fun2())



fun <- function(albatross, batman, courser) {
  match_call()
}
b <- runif(1000)
my.file <- tempfile()
x <- treeprof(fun(alb=matrix(1:100, nrow=10), bat=5, cour=b), file=my.file, target.time=1)
z <- treeprof:::parse_lines(my.file)

x.cp <- copy(x)
class(x.cp) <- "data.table"

my.expr <- quote(fun(alb=matrix(1:100, nrow=10), bat=5, cour=b))
lines <- treeprof:::run_rprof(
  my.expr, interval=0.001, target.time=1, times=NULL, file=my.file, frame=globalenv()
)
z1 <- treeprof:::parse_lines(my.file)
