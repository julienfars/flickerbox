getResultFileList <-
function (pfad=".") {
  # Return a list of result files in the directory path
  dname <- paste("_O([[:upper:]]{1})_201([[:digit:]]{1})-([[:digit:]]{2})-([[:digit:]]{2})_([[:alnum:]]*)\\.txt",sep="")
  dateien <- list.files(path=pfad,pattern=dname)
  return(dateien)
}
