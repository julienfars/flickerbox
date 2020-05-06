#' Read a single result file
#'
#' Use this function to read a single result file. Such a file contains all stimuli presented, the corresponding answers, and two thresholds. One threshold is from the up-, the other one from the down-staircase).
#' @param name name of the file
#' @param presets presets that are used for identifying conditions and calculating sensitivities
#' @return an object of the type "resultFile"
#' @examples
#' \dontrun{
#' # Get the maximal possible contrast at the photoreceptor level for this measurement (instrument gamut)
#' maxContrast <- resultFile("pathToFile")$maxContrast
#' }
#' @export

resultFile <- function(name, presets = flickerbox::presets) {
  
  # Define constants
  modulation <- 1:4
  phase <- 5:8
  zero <- rep(0, 4)
  treatNA = "keep"
  
  # read the resultFile into a table, shared with photoreceptorCoordinates.R
  rt <- read.resultFile(name)
  
  # Read contrasts
  kontraste <- rt[rt$category == "Delta Kontrast SC1", 3:10]
  
  if (sum(kontraste[1:4]) == 0)
  {
    kontraste <- kontraste[5:8]
  }
  else
  {
    kontraste <- kontraste[1:4]
  }
  
  Kontrast100 <- which.max(rt[rt$category == "Kontrast SC1",])
  
  # Determine frequency
  
  freq <- rt[which(grepl("Frequenz", rt[, 1])),
             which.max(rt[rt$category == "Kontrast SC1",])]
  
  # Determine photoreceptor type
  
  for (rtypes in names(presets)[-1]) {
    if (sum(abs(presets[modulation, rtypes] - kontraste)) < .1) {
      type <- rtypes
    }
  }
  
  if (is.na(type)) {
    warning("Cannot determine photoreceptor type.")
  }
  
  # Determine LED contrasts at threshold
  
  if (is.na(type)) {
    maxContrast <- NA
  } else {
    maxContrast <- presets[presets$X == "K", type]
  }
  
  # Determine the method used 
  if(any(rt=="Strategie:")) {
    set_Method <- "BP"
  } else {
    set_Method <- "SC"
  }
  
  if (set_Method == "SC"){
    thresholdsLED <-
      rbind(D = rt[which(grepl("Down: Schwelle", rt[, 1])), 3:6],
            U = rt[which(grepl("Up: Schwelle", rt[, 1])), 3:6])
  } else if (set_Method=="BP") {
    thresholdsLED <-
    rbind(D = rt[which(grepl("BestPEST: Schwelle", rt[, 1])), 3:6],
          U = rt[which(grepl("BestPEST: Schwelle", rt[, 1])), 3:6])
  }
  
  if (nrow(thresholdsLED) == 0) {
    warning("No staircase terminated.")
    termination.status <- 0
    thresholdsLED <- rbind(D = rep(NA, 4),
                           U = rep(NA, 4))
    if (treatNA == "gamut") {
      thresholdsLED <- rbind(D = rep(100, 4),
                             U = rep(100, 4))
      termination.status = "G"
    }
  } else {
    termination.status <- nrow(thresholdsLED)
  }
  
  thresholdsPR <-
    data.frame(thresholdsLED, apply(thresholdsLED, 1, max) * maxContrast / 100)
  
  sensitivities <-
    data.frame(thresholdsPR,
               Sensitivity = 1 / thresholdsPR[, 5],
               term = termination.status)
  
  names(sensitivities) <- c(
    "red",
    "green",
    "blue",
    "cyan",
    paste("contrast:", type),
    paste("sensitivity:", type),
    "Term"
  )
  
  notSeen <- rt$response == "nicht gesehen"
  seen <- rt$response == "gesehen"
  
  # Create resultTab list
  resultTab <- list()
  
  resultTab$name <- name
  resultTab$table <- rt
  resultTab$type <- type
  resultTab$frequency <- freq
  resultTab$maxContrast <- maxContrast
  resultTab$sensitivityTable <- sensitivities
  resultTab$thresholdsLED <- thresholdsLED
  resultTab$contrasts <- 5 * kontraste
  resultTab$thresholdsPR <- thresholdsPR
  resultTab$terminationStatus <- termination.status
  resultTab$Kontrast100 <- Kontrast100
  
  resultTab$sensitivity <-
    ifelse (is.na(type), NA, mean(sensitivities[, 6]))
  
  if (length(na.omit(rt$response)) == 0)
  {
    resultTab$numberOfQuestions <- 0
  }
  else
  {
    resultTab$numberOfQuestions <- sum(seen) + sum(notSeen)
  }
  
  class(resultTab) <- append(class(resultTab), "resultFile")
  
  return(resultTab)
}

#' @export
print.resultFile <- function(rfile) {
  output <- getBasics(rfile)
  sprintf("Contains data from subject %s, eye: %s, measured at %s.",
          output$subject,
          output$eye,
          output$dateTime)
}
