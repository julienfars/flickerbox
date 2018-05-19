#' Calculate LED contrasts
#' @examples # use conversion factors from spreadsheet
#' @examples lum2nrgB <- c(red = 0.0216,
#' @examples              green = 0.00155,
#' @examples              blue = 0.0242,
#' @examples              cyan = 0.0024)
#' @examples
#' @examples ## how to convert from luminance to energy: select method A or B
#' @examples lum2nrg <- lum2nrgB
#' @examples
#' @examples #### set given values
#' @examples ## mean luminance of primaries
#' @examples lmean <- c(red = 20,
#' @examples           green = 40,
#' @examples           blue = 3,
#' @examples           cyan = 20)
#' @examples ## pursued receptor contrast, actual value irrelevant
#' @examples pursuedContr <- c(rod = 0,
#' @examples                  scone = 0,
#' @examples                  mcone = 20,
#' @examples                  lcone = 0)
#' @examples # Find LED contrast that result in pursued contrasts
#' @examples LEDcontrast100 <- findLEDContrasts(pursuedContr, lmean, ConeFund)
#' @examples # Calculate the maximal photoreceptor contrast this results in
#' @examples maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
#' @examples print(round(maxContrast, 2))
#' @examples Kliste <-c()
#' @examples pursuedContr <- c(rod = 0,
#' @examples                 scone = 0,
#' @examples                  mcone = 10,
#' @examples                  lcone = 0)
#' @examples LEDcontrast100 <- findLEDContrasts(pursuedContr,
#' @examples                                   lmean,
#' @examples                                   ConeFund,
#' @examples                                   T)
#' @examples createPresetFile("e_L0Mp1", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
#' @examples maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
#' @examples Kliste <- c(Kliste, max(maxContrast))
#' @export
#'
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

#' @export
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

#' @export
prereceptoralFilter <- function(coneFund, filter) {
  ConeFundNew <- coneFund * 10 ^ -filter
  return(ConeFundNew / max(ConeFundNew))
}

mpod <- function(opticalDensity) {
  mp <- opticalDensity * MPOD[, 2] / max(MPOD[, 2])
  return(mp)
}

#' @export
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

##### This function calculates number closest to f with n fractional digits
# that can be expressed as binary values

ftb <- function (f, n = 4) {

  # fb: binary number (output)
  # fc: residuum
  fb <- 0
  fc <- f

  # use numbers 0.5, 0.25, ..., 2 ^ (-n)
  # subtract the highest multiple of this number from fc
  # add this number to fb
  for (i in 1:n) {
    digit <- 2 ^ (-i)
    fb <- fb + (fc %/% digit) * digit
    fc <- fc - (fc %/% digit) * digit
  }

  # add the last digit one more time, if (fb + 2 ^ (-n)) is closer to
  # f than fb
  if ((f - fb) > (digit / 2)) {
    fb <- fb + 2 ^ (-i)
  }

  return(fb)
}

FtoBinary <- Vectorize(ftb)
