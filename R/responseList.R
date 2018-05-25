##### class responseList #####
# extracts the single responses from a result list

#' @export
responseList <- function(x) UseMethod("responseList")

responseList.default <- function(x) print("Method not available for this object.")

responseList.character <- function (pfad = ".") {

  pfad %>%
    resultList() %>%
    responseList.resultList()

}

responseList.resultList <- function(resultList) {

  replaceInf <- 300 # Sensitivity that is assumed at zero contrast, just to show these responses also

  qualityTab <- lapply(resultList, function (x) { rbind(getSeen(x), getNotSeen(x)) })
  qualityTab <- do.call(rbind.data.frame, qualityTab)

  qualityTab <-
    qualityTab %>%
    mutate(Sensitivity = ifelse(Sensitivity == Inf, replaceInf, Sensitivity) )

  class(qualityTab) <- append(class(qualityTab), "responseList")

  qualityTab

}

summary.resultList <- function(resultList) {

  info <- list()

  infoList <-
    lapply(resultList, function(x) data.frame(getBasics(x))) %>%
    do.call("rbind.data.frame", .)

  subj <- unique(infoList$subject)
  if (length(subj) > 1) {
    stop("Different subjects are mixed up!")
    info$subject <- NA
  } else {
    info$subject <- as.character(subj)
  }

  eye <- unique(infoList$eye)
  if (length(eye) > 1) {
    stop("Different eyes are mixed up!")
    info$eye <- NA
  } else {
    info$eye <- as.character(eye)
  }

  times <- infoList$dateTime
  duration <- difftime(max(times), min(times), units = "mins")
  info$start <- max(times)
  info$duration <- as.numeric(duration, units = "mins")

  info
}
