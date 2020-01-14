#' Alternative method for reading result files.
#'
#' THis function reads a results file and calculates photoreceptor contrasts.
#' @param filename path to a result files
#' @param ConeFund cone fundamentals to be used
#' @return a table with contrasts at threshold at the photoreceptor level
#' @export

read.flimmerkiste <- function(filename, ConeFund)
{
  # read the resultFile into a table, shared with resultFile.R
  rt <- read.resultFile(filename)

  dat <- strsplit(filename, split = "/")[[1]]
  dat <- dat[length(dat)]
  dat <- strsplit(substr(dat, 1, nchar(dat) - 4), split = "_")[[1]]
  patid <- dat[1]
  seye <- dat[length(dat) - 2]
  date_of_exam <-  strptime(paste(dat[length(dat) - 1], dat[length(dat)]), "%Y-%m-%d %H%M%S")

  if (sum(grepl("Helligkeit", rt$category)) != 1) stop("No information on luminance.")
  if (sum(grepl("Phase", rt$category)) != 1) stop("No information on phase.")

  if(sum(rt[4:5, 3:6]) == 0) {
    field <- "surround"
    index <- c("surround_red", "surround_green", "surround_blue", "surround_cyan")
  }
  else {
    field <- "center"
    index <- c("center_red", "center_green", "center_blue", "center_cyan")
  }

  lmean <- c(rt[rt$category == "Helligkeit", index])
  phase <- c(rt[rt$category == "Phase", index])

  if (sum(grepl("Frequenz", rt$category)) != 1) {
    freq <- NA
  }
  else {
    freq <- c(rt[rt$category == "Frequenz", index])
    freq <- as.vector(unlist(freq))
    freq <- max(freq)
  }

  lmean <- as.vector(unlist(lmean))
  phase <- as.vector(unlist(phase))
  phase[phase == 180] <- -1
  phase[phase == 0] <- 1

  if ((sum(grepl("Down: Schwelle", rt[, 1])) > 0) &
      (sum(grepl("Up: Schwelle", rt[, 1])) > 0))
  {
    thresholdsLED <-
      rbind(D = rt[which(grepl("Down: Schwelle", rt[, 1])), 3:6] * phase,
            U = rt[which(grepl("Up: Schwelle", rt[, 1])), 3:6] * phase)
  } else
  {
    thresholdsLED <- rbind(D = rep(NA, 4),
                           U = rep(NA, 4))
  }

  LEDcontrast <- apply(thresholdsLED, 2, mean)

  return(
    data.frame(
      patid,
      seye,
      date_of_exam,
      frequency = freq,
      findPhotoreceptorContrasts(LEDcontrast, lmean, ConeFund)
    )
  )

}

#' Calculates the contrasts at threshold for the different photoreceptor levels
#'
#' The contrasts for all four photoreceptors at threshold are calculated from severel result files. This function does not rely on known presets.
#' @param path path to a directory containing multiple result files
#' @param ConeFund cone fundamentals to be used
#' @return a table with contrasts at threshold at the photoreceptor level
#' @export

getPhotoreceptorCoordinates <- function(path = ".", ConeFund = NA) {

  if(path == ".") sprintf("Using working directory: %s", getwd())

  stopifnot(!is.na(ConeFund))

  dateien <- getResultFileList(path)
  if (length(dateien) == 0) {
    warning(paste("No data in folder \"", path, "\"", sep = ""))
    erg <- NA
    return(erg)
  }

  erg <- list()

  for (i in 1:length(dateien)) {
    fname <- normalizePath(dateien[i])

    tryCatch(
      erg[[i]] <- read.flimmerkiste(fname, ConeFund),
      error = function(e)
      {
        warning(paste(fname, "was not loaded correctly."))
        print(e)
      }
    )
  }

  if(length(erg) < 1) {
    warning(paste("No data in folder \"", path, "\"", sep = ""))
    erg <- NA
    return(erg)
  }

  erg <- do.call(rbind, erg)

  return(erg)
}
