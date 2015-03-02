
df = NULL
cached_reader <- function(...) {
  if (is.null(df)) {
    df <<- read.csv(...)
  } else {
    message("using cached result")
  }
  df
}
