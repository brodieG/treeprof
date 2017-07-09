# Treeprof - Pre-release

*NOTE*: This is an in-development package.  It should work in most cases, but hasn't been extensively tested and likely contains bugs.  If enough interest develops for this package we will work to produce a stable release.

## Overview

Yet another R benchmarking package.  Here is why you might be interested in it:

* Unlike `microbenchmark`, it is suitable for benchmarking complex expressions
  or functions
* Like `lineprof` it is based on `Rprof`
* Unlike `Rprof` this tells you which calls are childrens of others; often in
  `Rprof` you can infer this based on the total execution times reported, but
  at times it is ambiguous
* Unlike `Rprof` or `lineprof` this will automatically repeat code that
  evaluates too quickly to register meaningfully through stack dumps, by default
  targeting a total evaluation time of 5 seconds.
* Unlike `lineprof`, this shows the entire call stack at once so you can see at
  a glance if a bottlenecks is occurring a few steps in, and you can easily keep
  track of what proportion of the total time a bottleneck deep in the call
  stack is taking.
* Unlike `ggprof` you can easily make out call names in deep and complex stacks

Here is some a snippet of output:

    rep---------------------------------------------- :  72 -   0
    |   rep.factor----------------------------------- :  72 -   0
    |       structure-------------------------------- :  55 -  46
    |       |   levels------------------------------- :   4 -   4
    |       |   %in%--------------------------------- :   2 -   1
    |       |   |   match---------------------------- :   1 -   1
    |       |   match-------------------------------- :   2 -   2
    |       |   class-------------------------------- :   1 -   1
    |       NextMethod------------------------------- :  17 -  17
    make.names--------------------------------------- :   2 -   1
    |   make.unique---------------------------------- :   1 -   1
    unclass------------------------------------------ :   2 -   2
    as.list------------------------------------------ :   3 -   2
    |   as.list.default------------------------------ :   1 -   1

Where the first column is the total time and the second is self time.

## Usage

### Basic Usage

Can't get much simpler:

    treeprof(bench_mark_me())

This will output the tree to screen.

### Shinify!

But the main use case for `treeprof` is through the Shiny interactive interface.
This is also very simple:

    x <- treeprof(bench_mark_me())
    shinyfy(x)

This will launch a Shiny app that allows you to explore your benchmarks in depth
and detail.
