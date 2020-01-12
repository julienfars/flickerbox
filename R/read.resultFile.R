read.resultFile <- function(filename)
{
  filename <- normalizePath(filename, winslash = "/")
  stopifnot(file.exists(filename))

  n_skip <- 0
  con <- file(filename, "r")
  found_luminance <- F
  while(!found_luminance) {
    next_line <- readLines(con, n = 1)
    if (length(next_line) == 0) {
      close(con)
      stop("Result file corrupt.")
    }
    if (!grepl("^Helligkeit", next_line)) {
      n_skip <- n_skip + 1;
    }
    else {
      found_luminance <- TRUE;
    }
  }
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

  stopifnot(ncol(rt) == 10)

  names(rt) <- c("category",
                 "response",
                 "center_red",
                 "center_green",
                 "center_blue",
                 "center_cyan",
                 "surround_red",
                 "surround_green",
                 "surround_blue",
                 "surround_cyan")

  return(rt)

}
