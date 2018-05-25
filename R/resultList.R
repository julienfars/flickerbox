##### Contstructor #####
# loads examinations files from a path and creates an object resultList

#' Loads examinations files from a path and creates an object resultList
#' @param path path that contains multiple resultFile files
#' @return an object of the type "resultList"
#' @export

resultList <- function(pfad = ".") {

  getResultFileList <-function (pfad=".") {
    # Return a list of result files in the directory path
    dname <- paste("_O([[:upper:]]{1})_201([[:digit:]]{1})-([[:digit:]]{2})-([[:digit:]]{2})_([[:alnum:]]*)\\.txt",sep="")
    dateien <- list.files(path=pfad,pattern=dname)
    return(dateien)
  }

  dateien <- getResultFileList(pfad)
  if (length(dateien) == 0) {
    warning(paste("No data in folder \"", pfad, "\"", sep = ""))
    erg <- NA
    class(erg) <- "resultListL4S"
    return(erg)
  }

  erg <- list()

  for (i in 1:length(dateien)) {
    fname <- normalizePath(paste(pfad, dateien[i], sep = "/"))
    erg[[i]] <- resultFile(fname, presets)
  }

  class(erg) <- append(class(erg), "resultList")
  return(erg)
}
