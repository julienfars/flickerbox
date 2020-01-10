##### Plots a loss diagramme #####

visugramme <- function(x, age, maxAge = NA) UseMethod("visugramme")

visugramme.default <- function(x, age, maxAge = NA) {
  print("Not a result list!")
}

visugramme.sensitivityList <- function(slist, age, maxAge = NA) {

  thresholdTable <-
    slist %>%
    ungroup() %>%
    mutate(logSens = log10(sensitivity),
           type = as.character(type),
           nv = calcNV(age, type, frequency),
           loss = -10 * (nv - logSens),
           minSens = log10(1 / maxContrast),
           maxLoss = -10 * (nv - minSens)
    )

  normValues <- NVprobsSmooth %>%
    filter(type %in% unique(thresholdTable$type))

  ggplot(thresholdTable, aes(x = frequency)) +
    geom_line(aes(y = loss, group = 1), size = 2) +
    geom_point(aes(y = loss, shape = factor(term)), size = 2) +
    geom_ribbon(aes(ymax = maxLoss, ymin = -15), alpha = .5) +

    scale_x_log10("Frequency [Hz]", breaks = c(1, 2, 4, 8, 28), limits = c(1, 28)) +
    scale_y_continuous("Sensitivity loss [dB]", limits = c(-15, 5)) +

    geom_ribbon(data = normValues, aes(x = frequency, ymin = p100, ymax = p900), alpha = .1) +
    geom_line(data = normValues, aes(x = frequency, y = p500)) +
    facet_wrap(~ type) +
    guides(shape = "none", color = "none")
}

visugramme.resultList <- function (resultList, age, maxAge = NA) {

  resultList %>%
    sensitivityList %>%
    visugramme.sensitivityList(age, maxAge = NA)

}

vg_gamut <- function(age) {
  geom_ribbon(data = normValues, aes(x = frequency, ymin = p10, ymax = p90), alpha = .1) +
    geom_line(data = normValues, aes(x = frequency, y = p50))

}

vg_boxplot <- function(resultList, age) {

  boxplotTable <-
    resultList %>%
    responseList() %>%
    filter(!is.na(Sensitivity)) %>%
    mutate(logSens = log10(Sensitivity),
           type = as.character(type),
           nv = calcNV(age, type, frequency),
           loss = -10 * (nv - logSens))

  geom_boxplot(data = boxplotTable,
               aes(x = as.numeric(frequency),
                   y = loss,
                   group = paste(as.character(frequency), answer),
                   color = answer),
               alpha = .1)

}

##### plotLM() #####
# plots L-/M-cone ratios

plotLM <- function(x) UseMethod("plotLM")

plotLM.default <- function (x) {
  print(paste("plotLM: method not available for class:", class(x)))
}

plotLM.sensitivityList <- function(sList) {
  tabLM <- sList %>%
    filter(type %in% c("L", "M")) %>%
    arrange(frequency, type) %>%
    group_by(frequency) %>%
    summarize(ratioLM = first(sensitivity) / last(sensitivity))

  ggplot(tabLM, aes(x = frequency, y = ratioLM)) +
    geom_smooth() +
    geom_point() +
    scale_x_log10()
}

plotLM.resultList <- function(rList) {
  rList %>%
    sensitivityList() %>%
    plotLM()
}

##### plotResults #####

#' @export

plot.sensitivityList <- function(slist) {

  slist <- slist[!is.na(slist$sensitivity), ]

  ggplot(slist, aes(x = frequency, y = sensitivity)) +
    geom_line(aes(group = 1)) +
    geom_point(aes(shape = factor(term))) +
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(~ factor(type, levels = c("L", "M", "S", "R")))

}

plot.resultList <- function(rlist) {

  rlist %>%
    sensitivityList %>%
    plot

}
