#' Calculates the contrasts at the photoreceptor levels for given contrasts and mean luminances of the LED
#'
#' For a given set of LED mean luminances and LED contrast, contrasts at the photoreceptor level can be calculated when the cone fundamentals are known.
#' @param LEDcontrast a vector of LED contrasts
#' @param lmean mean luminances of the LEDS
#' @param ConeFund cone fundamentals to be used
#' @return a vector with the contrasts at the photoreceptor level
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

  maxContrast
}
