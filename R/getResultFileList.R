#' Get a list of result files in the directory "path"
#' @param path Directory that is to be searched for result files
#' @return a list of file names
#' @author Cord Huchzermeyer
#' @export

getResultFileList <-
  function (path = ".") {
    # Return a list of result files in the directory path
    dname <-
      paste(
        "_O([[:upper:]]{1})_201([[:digit:]]{1})-([[:digit:]]{2})-([[:digit:]]{2})_([[:alnum:]]*)\\.txt",
        sep = ""
      )
    dateien <- list.files(path = path, pattern = dname)
    return(dateien)
  }
