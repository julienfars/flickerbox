#' Creates a sensitivity list from raw data
#' @param x either a string containing a path or a resultList-object
#' @seealso sensitivityList.character, sensitivityList.resultList
#' @export
sensitivityList <- function(x) UseMethod("sensitivityList")

#' Creates a sensitivity list from raw data
#' @param x any object
#' @details objects with a class other than character or resultList result only in a message that there is no method "sensitivityList" for this class.
#' @export
sensitivityList.default <- function(x) {

  print("Constructor sensitivityList: method not available for this class.")
  print(class(x))

}

#' Creates a sensitivity list from raw data
#' @param path directory containing a number of txt-files with temporal contrast sensitivity measurements
#' @export
sensitivityList.character <- function(path = ".") {

  resultList(
    sensitivityList(path)
  )

}

#' Creates a sensitivity list from raw data
#' @param resultList an object of class resultList
#' @export
#' @examples
#' rl <- resultList(".")
#' sl <- sensitivityList(rl)
#' print(sl)
sensitivityList.resultList <- function(resultList) {

  tab <- lapply(resultList, function (x) { data.frame(basics = getBasics(x),
                                                      type = x$type,
                                                      frequency = x$frequency,
                                                      sensitivity = x$sensitivity,
                                                      maxContrast = x$maxContrast,
                                                      C100 = max(x$contrasts),
                                                      Cthres = mean(apply(x$thresholdsLED, 1, max)),
                                                      term = x$terminationStatus) })
  tab <- do.call(rbind.data.frame, tab)
  tab <- tab[order(tab$type, tab$frequency), ]

  class(tab) <- append(class(tab), "sensitivityList")

  return(tab)

}
