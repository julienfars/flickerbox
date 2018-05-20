ellipseFunction <- function(k, a1, a2, alpha, returnValue = "squared") {
  if (k == 0) {
    cm <- 1 / sqrt(a1 ^ 2 * k ^ 2 + 2 * a1 * a2 * k * cos(alpha * pi / 180) +
                     a2 ^ 2)
    cl <- 0
  } else if (k == Inf) {
    cm <- 0
    cl <- cl <- 1 / a1
  } else {
    cm <- 1 / sqrt(a1 ^ 2 * k ^ 2 + 2 * a1 * a2 * k * cos(alpha * pi / 180) +
                     a2 ^ 2)
    cl <- k * cm
  }

  if (returnValue == "squared")
    return(sqrt(cm ^ 2 + cl ^ 2))
  else
    return(c(cm, cl))

}
ellipseFunction <- Vectorize(ellipseFunction)
