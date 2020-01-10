read.resultFile <- function(filename)
{
  filename <- normalizePath(filename, winslash = "/")
  stopifnot(file.exists(filename))

  n_skip <- 0
  con <- file(filename, "r")
  while(!grepl("^Helligkeit", readLines(con, n = 1))) n_skip <- n_skip + 1;
  close(con)

  rt <-
    read.table(
      filename,
      skip = n_skip,
      sep = ";",
      dec = ",",
      fill = T
    )
  rt <- rt[, c(-7, -12)]

  return(rt)

}
