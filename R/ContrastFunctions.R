findLEDContrasts <- function(pursuedContr, lmean, ConeFund, maxContrast = T) {
  ## calculate receptor catch for every LED -> 4x4 matrix
  CF <- crossprod(as.matrix(LED1[, 2:5]), as.matrix(ConeFund))
  
  ## calculate receptor catch for used conditions
  rinput <- c()
  for(i in 1:4) { 
    rinput <- rbind(rinput, 2 * lum2nrg[i] * lmean[i] * CF[i, ] / 100) 
  }
  rinput2 <- t(t(rinput) / apply(rinput, 2, sum))
  
  ## solve for pursued photoreceptor contrast
  LEDcontrasts <- crossprod(pursuedContr, solve(rinput2))
  
  ## set highest LED contrast to 100
  if (maxContrast){
    LEDcontrast100 <- LEDcontrasts * 100 / max(abs(LEDcontrasts))
  } else {
    LEDcontrast100 <- LEDcontrasts
  }
  
  return(LEDcontrast100)
}

findPhotoreceptorContrasts <- function(LEDcontrast, lmean, ConeFund) {
  ## calculate receptor catch for every LED -> 4x4 matrix
  CF <- crossprod(as.matrix(LED1[, 2:5]), as.matrix(ConeFund))
  
  ## calculate receptor catch for used conditions
  rinput <- c()
  for(i in 1:4) { 
    rinput <- rbind(rinput, 2 * lum2nrg[i] * lmean[i] * CF[i, ] / 100) 
  }
  rinput2 <- t(t(rinput) / apply(rinput, 2, sum))
  
  ## calculate  photoreceptor contrast
  maxContrast <- crossprod(as.vector(LEDcontrast), rinput2)
}

prereceptoralFilter <- function(coneFund, filter) {
  ConeFundNew <- coneFund * 10 ^ -filter
  return(ConeFundNew / max(ConeFundNew))
}

mpod <- function(opticalDensity) {
  mp <- opticalDensity * MPOD[, 2] / max(MPOD[, 2])
  return(mp)
}

shiftConeFundamentals <- function(ConeFund, receptor, dlambda) {
  if ((dlambda %% 2) > 0) {
    warning("Can only shift by multiples of 2!")
  }
  dlambda <- (dlambda %/% 2)
  ConeFundNew <- ConeFund
  ConeFundNew[, receptor] <- shift(ConeFundNew[, receptor], 
                                   dlambda, 
                                   mode = "zero")
  return(ConeFundNew)
}

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