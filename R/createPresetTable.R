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
