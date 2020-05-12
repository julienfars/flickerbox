# loads examinations files from a path and creates an object

#' Loads examinations files from a path and creates an object
#' @param path path that contains multiple result files
#' @return an object ready to get analysed by psychometric_table.R
#' @export

psychometric_list <- function(pfad = ".") {
  
  dateien <- getResultFileList(normalizePath(pfad))
  
  if (length(dateien) == 0) {
    warning(paste("No data in folder \"", pfad, "\"", sep = ""))
    erg <- NA
    return(erg)
  }
  
  erg <- list()
  
  for (i in 1:length(dateien)) {
    erg[[i]] <- psychometric_table(dateien[i], presets)
  }
  
  class(erg) <- append(class(erg),"psychometricList")
  return(erg)
}
