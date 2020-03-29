









resultFile <- function(name, presets = flickerbox::presets) {
  # Define constants
  modulation <- 1:4
  phase <- 5:8
  zero <- rep(0, 4)
  type <- NA
  treatNA = "keep"

  # Read a result file
  rt <-
    read.table(
      name,
      skip = 4,
      sep = ";",
      dec = ",",
      fill = T
    ) %>%
    dplyr::select(-7,-12)

  # Read contrasts

  kontraste <- rt[rt[, 1] == "Delta Kontrast SC1", 3:10]

  if (sum(kontraste[1:4]) == 0) {
    kontraste <- kontraste[5:8]
  } else {
    kontraste <- kontraste[1:4]
  }

  Kontrast100 <- which.max(rt[rt$V1 == "Kontrast SC1",])

  # Determine frequency

  freq <- rt[which(grepl("Frequenz", rt[, 1])),
             which.max(rt[rt$V1 == "Kontrast SC1",])]

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

  thresholdsLED <-
    rbind(D = rt[which(grepl("Down: Schwelle", rt[, 1])), 3:6],
          U = rt[which(grepl("Up: Schwelle", rt[, 1])), 3:6])

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

# get only the informations for the psychometric table

fulltable <- rt[-c(1:7),]
  table <- rt[-c(1:7),]
  fulltable <- na.omit(fulltable)
  fulltable[,1] <- as.factor(fulltable[,1])
  fulltable <- cbind.data.frame(fulltable[,2], fulltable[,7])
  colnames(fulltable) <- c("decision", "contrast")
  if (sum(any(rt=="Up: Schwelle erreicht!"), na.rm = TRUE) == 1){
    up <- table[which(grepl("Up: Schwelle", table[,1])),3]
    up <- cbind.data.frame(as.factor("gesehen"), up)
    colnames(up) <- c("decision", "contrast")
    fulltable <- rbind.data.frame(fulltable, up)
  } else if (sum(any(rt=="Up: Schwelle erreicht!"), na.rm = TRUE) == 0){
    up <- NaN
  }
  if (sum(any(rt=="Down: Schwelle erreicht!"), na.rm = TRUE) == 1){
    down <- table[which(grepl("Down: Schwelle", table[,1])),3]
    down <- cbind.data.frame(as.factor("gesehen"), down)
    colnames(down) <- c("decision", "contrast")
    fulltable <- rbind.data.frame(fulltable, down)
  } else if (sum(any(rt=="Down: Schwelle erreicht!"), na.rm = TRUE) == 0){
    down <- NaN
  }
  fulltable[,2] <- fulltable[,2]/max(fulltable[,2])
  

  # get informations like name and date time, see GetBasics function 
  
  name <- rfile$name
  dat <- strsplit(name, split = "/")[[1]]
  dat <- dat[length(dat)]
  dat <- strsplit(substr(dat, 1, nchar(dat) - 4), split = "_")[[1]]
  basics <- list()
  basics$subject <- dat[1]
  basics$dateTime <- strptime(paste(dat[length(dat) - 1], dat[length(dat)]), "%Y-%m-%d %H%M%S")
  basics

  # Create results of psychometrics in a dataframe
  resultTab <- data_frame()
  resultTab$decision <- fulltable$decision
  resultTab$contrast <- fulltable$contrast
  resultTab$subject <- basics$subject
  resultTab$dateTime <- basics$dateTime
  resultTab$type <- type
  resultTab$frequency <- freq


 #Still ongoing, I need to think about how to get informations from sensitivity

  resultTab$sensitivityTable <- sensitivities
  resultTab$thresholdsLED <- thresholdsLED
  resultTab$contrasts <- 5 * kontraste
  resultTab$thresholdsPR <- thresholdsPR
  resultTab$terminationStatus <- termination.status
  resultTab$Kontrast100 <- Kontrast100

  resultTab$sensitivity <-
    ifelse (is.na(type), NA, mean(sensitivities[, 6]))

  resultTab$numberOfQuestions <- sum(seen) + sum(notSeen)

  class(resultTab) <- append(class(resultTab), "resultFile")

  return(resultTab)
}
