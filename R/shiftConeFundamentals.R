#' Shifts the cone fundamentals of a given photoreceptor by a multiple of two
#'
#' Polymorphisms in the cone opsin genes can lead to a shift in the cone fundamentals of the affected photoreceptor. There are no known polymorphisms for rhodopsin, but the L-cone opsin is frequently affected. This function can be used to simulate such polymorphisms.
#' @param ConeFund the original cone fundamentals
#' @param receptor the affected photoreceptor type
#' @param dlambda the magnitude of the shift; must be a multiple of two
#' @return returns new cone fundamentals
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
