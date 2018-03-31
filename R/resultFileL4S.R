##### Class resultFile #####

resultFile <- function(name, presets) {
  
  # Define constants
  
  modulation <- 1:4
  phase <- 5:8
  zero <- rep(0,4)
  type <- NA
  treatNA = "keep"
  
  # Read a result file
  rt <- read.table(name, skip = 4, sep = ";", dec = ",", fill = T) %>%
    select(-7, -12)
  
  # Read contrasts
  
  kontraste <- rt[rt[, 1]=="Delta Kontrast SC1", 3:10]
  
  if (sum(kontraste[1:4]) == 0) { 
    kontraste <- kontraste[5:8] 
  } else { 
    kontraste <- kontraste[1:4] 
  }
  
  Kontrast100 <- which.max(rt[rt$V1 == "Kontrast SC1", ])
  
  # Determine frequency
  
  freq <- rt[which(grepl("Frequenz", rt[, 1])), 
             which.max(rt[rt$V1 == "Kontrast SC1", ])]
  
  # Determine photoreceptor type
  
  for (rtypes in names(presets)[-1]) { 
    if (sum(abs(presets[modulation, rtypes] - kontraste)) < .1) { 
      type <- rtypes
    }
  }
  
  if(is.na(type)) { 
    warning("Cannot determine photoreceptor type.")
  }
  
  # Determine LED contrasts at threshold
  
  if(is.na(type)) { 
    maxContrast <- NA
  } else {
    maxContrast <- presets[presets$X == "K", type]
  }
  
  thresholdsLED <- rbind(D = rt[which(grepl("Down: Schwelle", rt[, 1])), 3:6],
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
  
  thresholdsPR <- data.frame(thresholdsLED, apply(thresholdsLED, 1, max) * maxContrast / 100)
  sensitivities <- data.frame(thresholdsPR, Sensitivity = 1 / thresholdsPR[, 5], term = termination.status)
  
  names(sensitivities) <- c("red", "green", "blue", "cyan", 
                            paste("contrast:", type), 
                            paste("sensitivity:", type),
                            "Term")
  
  notSeen <- rt[, 2] == "nicht gesehen"
  seen <- rt[, 2] == "gesehen"
  
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
  
  resultTab$numberOfQuestions <- sum(seen) + sum(notSeen)

  class(resultTab) <- append(class(resultTab), "resultFile")
  
  return(resultTab)
}

##### function summary() for resultFile #####

summary.resultFile <- function(rfile) {
  print(rfile$name)
}

print.resultFile <- function(rfile) {
  rfile %>%
    getBasics() %>%
    print()
}

##### function getBasics() #####
# Extract data from name

getBasics <- function(x) UseMethod("getBasics")

getBasics.default <- function(x) {
  print(paste("getBasics: method not available for class:", class(x)))
  
}

getBasics.resultFile <- function(rfile) {
  name <- rfile$name
  
  dat <- strsplit(name, split = "/")[[1]]
  dat <- dat[length(dat)]
  dat <- strsplit(substr(dat, 1, nchar(dat) - 4), split = "_")[[1]]
  basics <- list()
  basics$subject <- dat[1]
  basics$eye <- dat[length(dat) - 2]
  basics$dateTime <- strptime(paste(dat[length(dat) - 1], dat[length(dat)]), "%Y-%m-%d %H%M%S")
  basics
}

##### getSeen() #####

getSeen <- function(x) UseMethod("getSeen")

getSeen.default <- function(x) {
  print(paste("getSeen: method not available for class:", class(x)))
}

getSeen.resultFile <- function(rfile) {
  attach(rfile)
  
  rt <- table
  seen <- rt[, 2] == "gesehen"
  
  seenTable <- rt[seen, c(1, Kontrast100)]
  seenTable[, 2] <- seenTable[, 2] * maxContrast / 100
  if(nrow(seenTable) == 0) {
    seenTable <- data.frame(a = NA, b = NA, c = NA, d = NA, e = NA, f = NA)
  } else {
    seenTable <- data.frame(type, frequency, seenTable, 
                            Sensitivity = 1 / seenTable[, 2], 
                            c("seen"))
  }
  names(seenTable) <- c("type", "frequency", "Staircase", "Threshold", "Sensitivity", "answer")
  detach(rfile)
  seenTable
}

##### getNotSeen() #####

getNotSeen <- function(x) UseMethod("getNotSeen")

getNotSeen.default <- function(x) {
  print(paste("getNotSeen: method not available for class:", class(x)))
}

getNotSeen.resultFile <- function(rfile) {
  
  attach(rfile)
  rt <- table
  notSeen <- rt[, 2] == "nicht gesehen"
  Kontrast100 <- Kontrast100
  
  notSeenTable <- rt[notSeen, c(1, Kontrast100)]
  notSeenTable[, 2] <- notSeenTable[, 2] * maxContrast / 100
  if(nrow(notSeenTable) == 0) {
    notSeenTable <- data.frame(a = NA, b = NA, c = NA, d = NA, e = NA, f = NA)
  } else {      
    notSeenTable <- data.frame(type, frequency, notSeenTable, 
                               Sensitivity = 1 / notSeenTable[, 2], 
                               c("notSeen"))}
  names(notSeenTable) <- c("type", "frequency", "Staircase", "Threshold", "Sensitivity", "answer")
  detach(rfile)
  notSeenTable
}
