# Navigation

## Cross Tree-Table interactivity

Clicking on entries in table should highlight relevant parts of tree

# Visual Cleanup

## Exclude `base` functions?

Don't dive into base functions?  Need to know which ones are passthrough 
functions that evaluate code, or functions that have an expression passed as an
argument which is evaluated inside the function.  Need to think about this more.

## Cleanup More passthrough functions

* withRestarts/CallingHandlers
* *pply

# Misc

* Address commentary about potential confusion based on grouping methodology
discussed in `printAndSplit` docs.
* Handle empty profiles (e.g. as created by typeof)

# Dependencies

* Remove data.table dependencies?  Unlikely, too convenient for processing, 
  unless we move to trees, but then would import igraph?

# line.profiling

Add back some of `lineprof` functionality?  Likely for versions > v1.0