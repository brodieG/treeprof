% Generated by roxygen2 (4.0.1): do not edit by hand
\docType{data}
\name{passthru_defined}
\alias{passthru_defined}
\title{List of Passthrus to Collapse}
\format{\preformatted{List of 2
 $ :List of 3
  ..$ enter       : chr "try"
  ..$ exit        : chr "doTryCatch"
  ..$ dont.exit.if: chr ""
  ..- attr(*, "class")= chr "passthru_fun"
 $ :List of 3
  ..$ enter       : chr "tryCatch"
  ..$ exit        : chr "doTryCatch"
  ..$ dont.exit.if: chr "tryCatchList"
  ..- attr(*, "class")= chr "passthru_fun"
}}
\usage{
passthru_defined
}
\description{
List of Passthrus to Collapse
}
\note{
order matters here (e.g. must remove "try" before "tryCatch")
}
\keyword{internal}

