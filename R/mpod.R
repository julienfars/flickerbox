mpod <- function(opticalDensity) {
  mp <- opticalDensity * MPOD[, 2] / max(MPOD[, 2])
  return(mp)
}
