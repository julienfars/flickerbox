#' @export
createPresetFile <- function (dname, LEDgruppe, LEDcontast100, innenLum, aussenLum) {
  phase <- (LEDcontrast100 < 0) * 180
  pfile <- cbind(t(abs(LEDcontrast100)), t(abs(LEDcontrast100 / 5)),
                 rep(0, 4), t(abs(LEDcontrast100 / 5)))
  if (LEDgruppe == "außen") {
    pfile <- rbind(cbind(rep(0, 4), rep(0, 4), rep(0, 4), rep(0, 4)), pfile)
  } else {
    pfile <- rbind(pfile, cbind(rep(0, 4), rep(0, 4), rep(0, 4), rep(0, 4)))
  }
  pfile <- data.frame(Y1 = rep("True", 8), Y2 = rep("Sinus", 8),
                      Y3 = rep(10, 8), Y4 = c(innenLum, aussenLum),
                      Y5 = c(rep(0, 4), phase), pfile)

  write.table(pfile, paste(dname, ".pre", sep = ""), eol = ";\r\n", quote = F, dec = ",", sep = ";", col.names = F, row.names = F)
}

