library(treeprof)
library(data.table)

# The following code was used to generate the file we will use for all the tests
# here.  Because the code that generates the file is inherently 
# non-deterministic we do not run it here, instead we just recover the file

# library(functools)
# library(treeprof)
# fun <- function(albatross, batman, courser) match_call()
# b <- runif(1000)
# my.file <- "matchcall.bench.out"
# x <- treeprof(fun(alb=matrix(1:100, nrow=10), bat=5, cour=b), file=my.file, target.time=20)
# zip("matchcall.bench.zip", "matchall.bench.out")
# file.remove("matchcall.bench.out")
# saveRDS(x, "matchcall.treeprof.RDS")

zip.data <- system.file("extdata/matchcall.bench.zip", package="treeprof")
unzip.dir <- tempdir()
unzip.paths <- unzip(zip.data, exdir=unzip.dir)
treeprof.ref <- readRDS(system.file("extdata/matchcall.treeprof.RDS", package="treeprof"))

unitizer_sect("Recreate treeprof from log", {
  invisible(prof.mx <- treeprof:::parse_lines(unzip.paths))    # cleanup / transform to matrix format 
  x <- treeprof:::melt_prof(prof.mx, 2L)                       # convert to long format / data.table
  attributes(x) <- attributes(treeprof.ref)                    # ad back attrs since they aren't created with the ad-hoc creation here
  all.equal(treeprof.ref, x)
  x           
} )
<<<<<<< HEAD
<<<<<<< HEAD
=======

>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
unitizer_sect("Node management", {
  treeprof.ref.1 <- readRDS(system.file("extdata/type_alike.treeprof.RDS", package="treeprof"))
  treeprof.ref.2 <- readRDS(system.file("extdata/doubletry.treeprof.RDS", package="treeprof"))

  treeprof:::get_child_nodes(treeprof.ref, 23L)
  passthru <- treeprof:::passthru_fun("try", "doTryCatch", "")
  passthru
  treeprof:::collapse_passthru_fun(treeprof.ref, passthru)  # get rid of `try`
  treeprof:::collapse_passthru_funs(treeprof.ref)           # get rid of all passthrus (though doesn't actually test "tryCatch" removal)
  treeprof:::collapse_passthru_funs(treeprof.ref.2)         # this has a `tryCatch` to remove (though no `try`)
  treeprof:::get_par_nodes(treeprof.ref)                    # for each node, the parent 
  sort(treeprof.ref, decreasing=TRUE)                       # branches sorted from fastest to slowest recursively
  treeprof:::trim_branch_fast(treeprof.ref, 1L, 5L)         # remove fast branches
  treeprof:::trim_branch_fast(treeprof.ref, 35L, 5L)        # Should unfurl all children of `try`, though actual screen output won't show it  

  print(treeprof.ref.1) # `tryCatch`
  print(treeprof.ref.2) # nested `try` functions caused problems before 
} )
unitizer_sect("Time Formatting", {
  vec <- c(.015, .015 * .005, .000001, 0.000234, 0.000074)
  treeprof:::time_scale(vec)
  treeprof:::time_scale(vec, scale="kiloseconds")
  treeprof:::time_scale(vec, decimals.max=1L)
  treeprof:::time_format(treeprof:::time_scale(vec))
  treeprof:::time_format(treeprof:::time_scale(vec), signif=3L)
  treeprof:::time_format(treeprof:::time_scale(vec), inc.units=FALSE)
  treeprof:::time_format(treeprof:::time_scale(vec), trim=FALSE)
  treeprof:::time_scale(0.00001861) # make sure this shows up as microseconds
} )
unitizer_sect("Normalize Times", {
  treeprof:::normalize(treeprof.ref, "auto")
  treeprof:::normalize(treeprof.ref, "milliseconds")
} )
unitizer_sect("By Function Summary Table", {
  by_fun(treeprof:::normalize(treeprof.ref, "auto"))
  by_fun(treeprof:::normalize(treeprof.ref.1, "auto"))
  by_fun(treeprof:::normalize(treeprof.ref.2, "auto"))
} )
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
unitizer_sect("Verbosity", # Don't care so much about value since non-deterministic, want to check stderr()
  compare=unitizerItemTestsFuns(value=function(x, y) TRUE, message=all.equal),
  {
    treeprof(data.frame(a=1:1000, b=1:1000), target.time=1)
    treeprof(data.frame(a=1:1000, b=1:1000), target.time=1, verbose=FALSE)
} )
<<<<<<< HEAD
=======

>>>>>>> 75d32eb92dbce607391b8bcf59be3a129015cdd0
=======
>>>>>>> 2a504db3379c01ea7079dc7c05a7fc2800124694
