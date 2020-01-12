##### function summary() for resultFile #####


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
