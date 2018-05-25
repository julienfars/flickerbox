#' Extracts x- and y- data for visualization of photoreceptor dependencies from a sensitivity list
#'
#' @examples
#' \dontrun{
#' xyData <- ellipseConstructor(examplePath)
#' eModel <- fitModel(xyData)
#' print(eModel$results$alphaEstimate)
#' plotEllipse(xyData, ellipseModel = eModel)
#' }
#' @export

ellipseConstructor <- function(path = ".")
{
  getM <- function(type_string) { return(flickerbox::Mkey[[type_string]]) }
  getM <- Vectorize(getM)
  getL <- function(type_string) { return(flickerbox::Lkey[[type_string]]) }
  getL <- Vectorize(getL)

  t1 <- sensitivityList(flickerbox::resultList(path))

  ellipse <- dplyr::filter(t1, C100 <= 100)

  ellipse$L <- getL(ellipse$type) * 10
  ellipse$M <- getM(ellipse$type) * 10

  ellipse$X <- ellipse$M * ellipse$Cthres / ellipse$C100
  ellipse$Y <- ellipse$L * ellipse$Cthres / ellipse$C100

  class(ellipse) <- c(class(ellipse), "ellipse")
  return(ellipse)
}
