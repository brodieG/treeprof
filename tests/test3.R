myfun <- function() {
  Sys.sleep(0.2)
  a <- runif(1e4)
  b <- sample(1e4)
  df <- data.frame(a, b)
  splt.vec <- rep(1:10, 1000)
  df.split <- split(df, splt.vec)
  unsplit(df.split, splt.vec)
}