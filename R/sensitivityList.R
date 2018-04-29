#' Creates a sensitivity list from raw data
#' @export
#' @import dplyr
sensitivityList <- function(x) UseMethod("sensitivityList")

sensitivityList.default <- function(x) {

  print("Constructor sensitivityList: method not available for this method.")
  print(class(x))

}

sensitivityList.character <- function(pfad = ".") {

  pfad %>%
    resultList() %>%
    sensitivityList()

}

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
