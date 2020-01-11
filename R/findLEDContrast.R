#' Calculate LED contrasts
#'
#' Calculates the LED contrast that result in a pursued contrast at the photoreceptor level, given certain time-averaged LED illuminances and given cone fundamentals.
#' @param pursuedContrast the contrast in the photoreceptors that should be achieved
#' @param lmean time-averaged luminances of the LEDs
#' @param ConeFund cone funamentals to be used
#' @param maxContrast use the maximal contrasts at the photoreceptor levels that can be achieved technically while keeping the contrast ratios from pursuedContrast constant?
#' @return LED contrasts that result in the desired contrasts at the photoreceptor level
#' @examples
#' \dontrun{
#' #### set given values
#' ## mean luminance of primaries
#' lmean <- c(red = 20, green = 40, blue = 3, cyan = 20)
#' ## create M-cone isolating stimlui
#' pursuedContr <- c(rod = 0, scone = 0, mcone = 20, lcone = 0)
#' ## Find LED contrast that result in pursued contrasts
#' LEDcontrast100 <- findLEDContrasts(pursuedContr, lmean, ConeFund)
#' # Calculate the maximal photoreceptor contrast this results in
#' maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
#' createPresetFile("mcone", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
#' }
#' @export

findLEDContrasts <- function(pursuedContr, lmean, ConeFund, maxContrast = T) {

  if (length(intersect(c("rod", "scone", "mcone", "lcone"), names(pursuedContr))) < 4)
    stop("Please name photoreceptors in pursuedContr!")
  else
    pursuedContr <- pursuedContr[c("rod", "scone", "mcone", "lcone")]

  if (length(intersect(c("red", "green", "blue", "cyan"), names(lmean))) < 4)
    stop("Please name LEDs in lmean!")
  else
    lmean <- lmean[c("red", "green", "blue", "cyan")]

  ## calculate receptor catch for every LED -> 4x4 matrix
  CF <- crossprod(as.matrix(LED1[, 2:5]), as.matrix(ConeFund))

  ## calculate receptor catch for used conditions
  rinput <- c()
  for(i in 1:4) {
    rinput <- rbind(rinput, 2 * flickerbox::lum2nrg[i] * lmean[i] * CF[i, ] / 100)
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
