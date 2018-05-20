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
