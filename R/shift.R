shift <- function(vector, k, mode = "standard") {
  # if k is negative perform positive shift on reversed vector
  negativeShift <- F
  if (k < 0) {
    k <- (-k)
    vector <- rev(vector)
    negativeShift <- T
  }

  # perform shift
  l <- length(vector)
  vnew <- vector
  vnew[(k + 1):l] <- vector[1:(l - k)]

  # what is shifted into the vector?
  if (mode == "NA") {
    vnew[1:k] <- NA
  } else if (mode == "zero") {
    vnew[1:k] <- 0
  } else if (mode == "standard") {
    vnew[1:k] <- vector[(l - k + 1):l]
  }

  # reverse vnew if k was negative
  if(negativeShift) {
    vnew <- rev(vnew)
  }

  return(vnew)
}
