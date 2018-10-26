read.flimmerkiste <- function(filename, ConeFund)
{
  filename <- normalizePath(filename, winslash = "/")

  rt <-
    read.table(
      filename,
      skip = 4,
      sep = ";",
      dec = ",",
      fill = T
    ) %>%
    dplyr::select(-7,-12)


  dat <- strsplit(filename, split = "/")[[1]]
  dat <- dat[length(dat)]
  dat <- strsplit(substr(dat, 1, nchar(dat) - 4), split = "_")[[1]]
  patid <- dat[1]
  seye <- dat[length(dat) - 2]
  date_of_exam <-  strptime(paste(dat[length(dat) - 1], dat[length(dat)]), "%Y-%m-%d %H%M%S")

  if(sum(rt[4:5, 3:6]) == 0) field <- "surround" else field <- "center"

  if(field == "surround")
  {
    lmean <- c(rt[1, 7:10])
    phase <- c(rt[3, 7:10])
    freq <- c(rt[2, 7:10])
  }
  else {
    lmean <- c(rt[1, 3:6])
    phase <- c(rt[3, 3:6])
    freq <- c(rt[2, 3:6])
  }
  lmean <- as.vector(unlist(lmean))
  phase <- as.vector(unlist(phase))
  phase[phase == 180] <- -1
  phase[phase == 0] <- 1
  freq <- as.vector(unlist(freq))

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
      frequency = max(freq),
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

getPhotoreceptorCoordinates <- function(path = ".", ConeFund) {

  getResultFileList <-function (pfad=".") {
    # Return a list of result files in the directory path
    dname <- paste("_O([[:upper:]]{1})_201([[:digit:]]{1})-([[:digit:]]{2})-([[:digit:]]{2})_([[:alnum:]]*)\\.txt",sep="")
    dateien <- list.files(path=pfad,pattern=dname)
    return(dateien)
  }

  dateien <- getResultFileList(path)
  if (length(dateien) == 0) {
    warning(paste("No data in folder \"", path, "\"", sep = ""))
    erg <- NA
    return(erg)
  }

  erg <- list()

  for (i in 1:length(dateien)) {
    fname <- normalizePath(paste(path, dateien[i], sep = "/"))
    erg[[i]] <- read.flimmerkiste(fname, ConeFund)
  }

  erg <- do.call(rbind, erg)

  return(erg)
}
