load("../normalsubjects/calcNV.Rda")
load("../normalsubjects/percentilesNV.Rda")
load("presets2.rda")

testNormal <- resultListL4S("Normalprobanden/01/0.3/")

testStargardt <- resultListL4S("Stargardt/55/0.3/")
testStargardt$lossPlot(38)

lossPlot <- function (sensTab, age) {
  sensTab %>%
    mutate(logSens = log10(sensitivity),
           type = as.character(type),
           nv = calcNV(age, type, frequency),
           loss = -10 * (nv - logSens)) %>%
    ggplot(aes(x = frequency)) +
    geom_smooth(aes(color = type, y = loss), se = F) +
    scale_x_log10() +
    scale_y_continuous(limits = c(-15, 5)) +
    geom_ribbon(data = NVprobsSmooth, aes(x = frequency, ymin = p10, ymax = p90), alpha = .1) +
    geom_line(data = NVprobsSmooth, aes(x = frequency, y = p50)) + 
    facet_wrap(~ type)
}

lossPlot(testNormal$table, 37)
lossPlot(testStargardt$table, 25)

stgdList <- data.frame(patnr = c(38, 45, 50, 55, 58, 64, 66, 113, 117, 118, 122),
                       age = c(62, 31, 54, 27, 24, 40, 38, 49, 38, 63, 61))

md <- data.frame()
for (i in 1:11) {
  list <- resultListL4S(paste("Stargardt/", stgdList[i, 1], "/0.3/", sep = ""))
  md <- rbind(md, data.frame(patnr = stgdList[i, 1], list$lossPlot(stgdList[i, 2])))
}
md %>%
  spread(type, md) %>%
  write.csv2(file = "md.csv")

md2 <- 
md %>%
  spread(type, md)

stgd38 <- resultListL4S("Stargardt/38/0.3/")
md38 <- stgd38$lossPlot(62)
stgd45 <- resultListL4S("Stargardt/45/0.3/")
stgd45$lossPlot(31)
stgd50 <- resultListL4S("Stargardt/50/0.3/")
stgd50$lossPlot(54)
stgd55 <- resultListL4S("Stargardt/55/0.3/")
stgd55$lossPlot(27)
stgd58 <- resultListL4S("Stargardt/58_2/0.3/")
stgd58$lossPlot(24)
stgd64 <- resultListL4S("Stargardt/64/0.3/")
stgd64$lossPlot(40)
stgd66 <- resultListL4S("Stargardt/66/0.3/")
stgd66$lossPlot(38)
stgd113 <- resultListL4S("Stargardt/113/0.3/")
stgd113$lossPlot(49)
stgd117 <- resultListL4S("Stargardt/117/0.3/")
stgd117$lossPlot(38)
stgd118 <- resultListL4S("Stargardt/118/0.3/")
stgd118$lossPlot(63)
stgd122 <- resultListL4S("Stargardt/122/0.3/")
stgd122$lossPlot(61)

rp57 <- resultListL4S("RP/57/0.3/")
rp57$lossPlot(22)
rp59 <- resultListL4S("RP/59/0.3/")
rp59$lossPlot(41)
rp60 <- resultListL4S("RP/60/0.3/")
rp60$lossPlot(21)
rp61 <- resultListL4S("RP/61/0.3/")
rp61$lossPlot(20)
rp62 <- resultListL4S("RP/62/0.3/")
rp62$lossPlot(20)
rp63 <- resultListL4S("RP/63/0.3/")
rp63$lossPlot(20)

ggplot(NVprobsSmooth, aes(x = frequency)) +
  geom_ribbon(data = NVprobsSmooth, aes(x = frequencyymin = p10, ymax = p90), alpha = .1) +
  geom_line(aes(y = p50))
geom_ribbon(data = NVprobsSmooth, aes(x = frequencyymin = p10, ymax = p90), alpha = .1) +