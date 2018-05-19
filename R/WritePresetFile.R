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

#' @export
createPresetTable <- function (pfilesList, Kliste) {

  pTabelle <- data.frame(X = c("modRED", "modGREEN", "modBLUE", "modCYAN",
                               "phaseRED", "phaseGREEN", "phaseBLUE", "phaseCYAN",
                               "K", "lty"))
  for (dname in pfilesList) {
    aktTab <- read.table(paste(dname, ".pre", sep = ""), dec = ",", sep = ";")
    if (sum(aktTab$V7[1:4]) == 0) {
      LEDgruppe <- 5:8
    } else {
      LEDgruppe <- 1:4
    }
    spalte <- c(aktTab$V7[LEDgruppe], aktTab$V5[LEDgruppe], 0.2, 1)
    pTabelle <- data.frame(pTabelle, TMP = spalte)
    names(pTabelle)[names(pTabelle) == "TMP"] <- dname
    # pTabelle[9, ] <- Kliste
  }
  pTabelle[9, ] <- c("K", Kliste)
  pTabelle <- data.frame(pTabelle)
  write.table(pTabelle, "presets.csv", sep = ";", quote = F)
  return(pTabelle)
}

