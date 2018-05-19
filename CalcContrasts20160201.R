## calculate conversion luminance to energy
# calculate conversion factors from luminance to energy
lum <- crossprod(as.matrix(flickerbox::LED1[, 2:5]),
                 as.matrix(flickerbox::CF_vlambda$Vlambda))
lum2nrgA <- t(1 / lum / 683)

# use conversion factors from spreadsheet
lum2nrgB <- c(red = 0.0216,
           green = 0.00155,
           blue = 0.0242,
           cyan = 0.0024)

## how to convert from luminance to energy: select method A or B
lum2nrg <- lum2nrgB

#### set given values
## mean luminance of primaries
lmean <- c(red = 20,
          green = 40,
          blue = 3,
          cyan = 20)
## pursued receptor contrast, actual value irrelevant
pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 20,
                  lcone = 0)

# Find LED contrast that result in pursued contrasts
LEDcontrast100 <- findLEDContrasts(pursuedContr, lmean, ConeFund)

# Calculate the maximal photoreceptor contrast this results in
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
print(round(maxContrast, 2))

Kliste <-c()
pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 10,
                  lcone = 0)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_L0Mp1", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 20,
                  lcone = 10)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_Lp1Mp2", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 10,
                  lcone = 10)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_Lp1Mp1", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 10,
                  lcone = 20)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_Lp2Mp1", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 0,
                  lcone = 10)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_Lp1M0", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = -10,
                  lcone = 20)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_Lp2Mm1", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = -10,
                  lcone = 10)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_Lp1Mm1", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = -20,
                  lcone = 10)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("e_Lp1Mm2", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
Kliste <- c(Kliste, max(maxContrast))


test <- createPresetTable(c("e_L0Mp1", "e_Lp1Mp2", "e_Lp1Mp1", "e_Lp2Mp1", "e_Lp1M0", "e_Lp2Mm1", "e_Lp1Mm1", "e_Lp1Mm2"), Kliste / 100)













maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
print(LEDcontrast100)
print(maxContrast)
print(max(maxContrast))
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 20,
                  lcone = 0)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   F)
createPresetFile("mconePlus0L", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
print(LEDcontrast100)
print(maxContrast)
print(max(maxContrast))
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 20,
                  lcone = 0)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   shiftConeFundamentals(ConeFund,
                                                         "lcone",
                                                         +6),
                                   F)
createPresetFile("mconePlus6L", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
print(LEDcontrast100)
print(maxContrast)
print(max(maxContrast))
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 0,
                  lcone = 20)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   shiftConeFundamentals(ConeFund,
                                                         "lcone",
                                                         -6),
                                   T)
createPresetFile("lconeMinus6L", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
print(LEDcontrast100)
print(maxContrast)
print(max(maxContrast))
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 0,
                  lcone = 20)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   ConeFund,
                                   T)
createPresetFile("lconePlus0L", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
print(LEDcontrast100)
print(maxContrast)
print(max(maxContrast))
Kliste <- c(Kliste, max(maxContrast))

pursuedContr <- c(rod = 0,
                  scone = 0,
                  mcone = 0,
                  lcone = 20)
LEDcontrast100 <- findLEDContrasts(pursuedContr,
                                   lmean,
                                   shiftConeFundamentals(ConeFund,
                                                         "lcone",
                                                         +6),
                                   T)
createPresetFile("lconePlus6L", "außen", LEDcontrast100, c(20, 40, 3, 20), lmean)
maxContrast <- findPhotoreceptorContrasts(LEDcontrast100, lmean, ConeFund)
print(LEDcontrast100)
print(maxContrast)
print(max(maxContrast))
Kliste <- c(Kliste, max(maxContrast))



createPresetTable(c("mconeMinus6L", "mconePlus0L", "mconePlus6L", "lconeMinus6L", "lconePlus0L", "lconePlus6L"), Kliste / 100)
