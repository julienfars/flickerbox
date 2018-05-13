#' @export
ellipseConstructor <- function(path = ".")
{
  t1 <- flickerbox::resultList(path) %>% sensitivityList
  
  ellipse <- t1 %>%
    filter(C100 <= 100)
  
  ellipse$L <- getL(ellipse$type) * 10
  ellipse$M <- getM(ellipse$type) * 10
  
  ellipse$X <- ellipse$M * ellipse$Cthres / ellipse$C100
  ellipse$Y <- ellipse$L * ellipse$Cthres / ellipse$C100
  
  class(ellipse) <- c(class(ellipse), "ellipse")
  return(ellipse)
}