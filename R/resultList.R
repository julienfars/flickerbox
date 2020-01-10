##### Contstructor #####
# loads examinations files from a path and creates an object resultList

#' Loads examinations files from a path and creates an object resultList
#' @param path path that contains multiple resultFile files
#' @return an object of the type "resultList"
#' @export

resultList <- function(pfad = ".") {

  dateien <- getResultFileList(normalizePath(pfad))

  if (length(dateien) == 0) {
    warning(paste("No data in folder \"", pfad, "\"", sep = ""))
    erg <- NA
    class(erg) <- "resultList"
    return(erg)
  }

  erg <- list()

  for (i in 1:length(dateien)) {
    erg[[i]] <- resultFile(dateien[i], presets)
  }

  class(erg) <- append(class(erg), "resultList")
  return(erg)
}
