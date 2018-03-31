#' @import dplyr
#' @import ggplot2

##### This script defines a class resultList with several methods to extract data from it, #####
# further analysis is done using methods derived from functional programming

# Classes:
# --------
# resultList
# responseList
# sensitivityList
# defectList

# Methods:
# --------
# plot()
# summary()
# setToGamut()
# visugramme()
# setToMean()
# removeNA()
# removeDuplicateNA()
# plotLM()

##### Contstructor #####
# loads examinations files from a path and creates an object resultList

resultList <- function(pfad = ".") {

  getResultFileList <-function (pfad=".") {
    # Return a list of result files in the directory path
    dname <- paste("_O([[:upper:]]{1})_201([[:digit:]]{1})-([[:digit:]]{2})-([[:digit:]]{2})_([[:alnum:]]*)\\.txt",sep="")
    dateien <- list.files(path=pfad,pattern=dname)
    return(dateien)
  }

  dateien <- getResultFileList(pfad)
  if (length(dateien) == 0) {
    warning(paste("No data in folder \"", pfad, "\"", sep = ""))
    erg <- NA
    class(erg) <- "resultListL4S"
    return(erg)
  }

  erg <- list()

  for (i in 1:length(dateien)) {
    fname <- normalizePath(paste(pfad, dateien[i], sep = "/"))
    erg[[i]] <- resultFile(fname, presets)
  }

  class(erg) <- append(class(erg), "resultList")
  return(erg)
}

##### class responseList #####
# extracts the single responses from a result list

responseList <- function(x) UseMethod("responseList")

responseList.default <- function(x) print("Method not available for this object.")

responseList.character <- function (pfad = ".") {

  pfad %>%
    resultList() %>%
    responseList.resultList()

}

responseList.resultList <- function(resultList) {

  replaceInf <- 300 # Sensitivity that is assumed at zero contrast, just to show these responses also

  qualityTab <- lapply(resultList, function (x) { rbind(getSeen(x), getNotSeen(x)) })
  qualityTab <- do.call(rbind.data.frame, qualityTab)

  qualityTab <-
    qualityTab %>%
    mutate(Sensitivity = ifelse(Sensitivity == Inf, replaceInf, Sensitivity) )

  class(qualityTab) <- append(class(qualityTab), "responseList")

  qualityTab

}

##### sensitivityList #####
# extracts sensitivities from result list

sensitivityList <- function(x) UseMethod("sensitivityList")

sensitivityList.default <- function(x) {

  print("Constructor sensitivityList: method not available for this method.")
  print(class(x))

}

sensitivityList.character <- function(pfad = ".") {

  pfad %>%
    resultList() %>%
    sensitivityList()

}

sensitivityList.resultList <- function(resultList) {

  tab <- lapply(resultList, function (x) { data.frame(basics = getBasics(x),
                                               type = x$type,
                                               frequency = x$frequency,
                                               sensitivity = x$sensitivity,
                                               maxContrast = x$maxContrast,
                                               C100 = max(x$contrasts),
                                               Cthres = mean(apply(x$thresholdsLED, 1, max)),
                                               term = x$terminationStatus) })
  tab <- do.call(rbind.data.frame, tab)
  tab <- tab[order(tab$type, tab$frequency), ]

  class(tab) <- append(class(tab), "sensitivityList")

  return(tab)

}

##### Functions to modify sensitivityLists #####

setToGamut <- function(x) UseMethod("setToGamut")

setToGamut.default <- function(x) {
  print("Not a sensitivityList!")
}

setToGamut.sensitivityList <- function(slist,
                                       types = c("L", "M", "S", "R"),
                                       frequencies = c(1, 2, 4, 6, 8, 10, 12, 16, 20, 28))
{

  output <-
  slist %>%
    mutate(setToGamut = (type %in% types) &
             (frequency %in% frequencies) &
             (is.na(sensitivity)),
           sensitivity = ifelse(setToGamut,
                                1 / maxContrast,
                                sensitivity)
    )
  class(output) <- append(class(output), "sensitivityList")

  output

}

# removes NAs if valid measurements are available for this condition
# Not working, yet!

removeDuplicateNA <- function(x) UseMethod("removeDuplicateNA")

removeDuplicateNA.default <- function(x) {
  print("Object not of class sensitivityList")
}

removeDuplicateNA.sensitivityList <- function(slist) {

  output <-
    slist %>%
    group_by(frequency, type) %>%
    mutate(dplct = n() > 1,
           isNA = is.na(sensitivity),
           validMeas = !min(isNA)) %>%
    ungroup %>%
    filter(!(isNA & dplct & validMeas)) %>%
    select(-dplct, -isNA, -validMeas) %>%
    data.frame()

  class(output) <- append(class(output), "sensitivityList")

  output
}

# Remove NA measurement

removeNA <- function(x, rType, rFrequency) UseMethod("removeNA")

removeNA.default <- function(x, rType, rFrequency) {
  print("Not a sensitivity list!")
}

removeNA.sensitivityList <- function(slist, rType, rFrequency) {

  output <-
    slist %>%
    filter(!((type == rType) & (frequency == rFrequency) & is.na(sensitivity)))

  class(output) <- append(class(output), "sensitivityList")

  output
}

# Set sensitivitity to mean, when several valid measurements were made

setToMean <- function(x, rType, rFrequency) UseMethod("setToMean")

setToMean.default <- function(x, rType, rFrequency) {
  print(paste("setToMean: method not available for class:", class(x)))
}

setToMean.sensitivityList <- function(slist, rType, rFrequency) {

  modify <-
    slist %>%
    filter(type == rType, frequency == rFrequency) %>%
    summarize(meanSens = mean(sensitivity))

  output <-
    slist %>%
    group_by(type, frequency) %>%
    mutate(nMeas = row_number(),
           meanSens = mean(sensitivity),
           sensitivity = if_else((type == rType) &
                                   (frequency == rFrequency) &
                                   (nMeas == 1),
                                 meanSens,
                                 sensitivity)
    ) %>%
    filter(!((type == rType) &
               (frequency == rFrequency) &
               (nMeas > 1))) %>%
    select(-nMeas, -meanSens) %>%
    ungroup()

  class(output) <- append(class(output), "sensitivityList")

  output
}

# Add a line for each condition that was not measured, because frequency was above CFF

completeList <- function(x, rType, maxFreq = NA) UseMethod("completeList")

completeList.default <- function(x, rType, maxFreq = NA) {
  print(paste("completeList: method not available for class:", class(x)))
}

completeList.sensitivityList <- function(slist, rType, maxFreq = NA) {

  if (is.na(maxFreq)) maxFreq <- max(slist$frequency)
  if (maxFreq > 36) maxFreq <- 36

  availableFrequencies <-
    slist %>%
    filter(type == rType) %>%
    select(frequency) %>%
    unlist %>%
    as.vector

  carryOver <-
    slist %>%
    filter(type == rType) %>%
    summarize(maxContrast = first(maxContrast),
              basics.subject = first(basics.subject),
              basics.eye = first(basics.eye))

  addFreq <-
    data.frame(type = rType,
               frequency = c(1, 2, 4, 6, 8, 10, 12, 16, 20, 28, 36),
               carryOver,
               term = -1) %>%
    filter(frequency <= maxFreq,
           !(frequency %in% availableFrequencies))

  output <- full_join(addFreq, slist)

  class(output) <- append(class(output), "sensitivityList")

  return(output)

}

##### plotResults #####

plot.sensitivityList <- function(slist) {

  slist %>%
    filter(!is.na(sensitivity)) %>%
    ggplot(aes(x = frequency, y = sensitivity)) +
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

##### getInfo() #####

summary.resultList <- function(resultList) {

  info <- list()

  infoList <-
    lapply(resultList, function(x) data.frame(getBasics(x))) %>%
    do.call("rbind.data.frame", .)

  subj <- unique(infoList$subject)
  if (length(subj) > 1) {
    stop("Different subjects are mixed up!")
    info$subject <- NA
  } else {
    info$subject <- as.character(subj)
  }

  eye <- unique(infoList$eye)
  if (length(eye) > 1) {
    stop("Different eyes are mixed up!")
    info$eye <- NA
  } else {
    info$eye <- as.character(eye)
  }

  times <- infoList$dateTime
  duration <- difftime(max(times), min(times), units = "mins")
  info$start <- max(times)
  info$duration <- as.numeric(duration, units = "mins")

  info
}

##### class defectList #####
# inherits from sensitivityList

defectList <- function(x, age, maxAge) UseMethod("defectList")

defectList.default <- function(x, age, maxAge) {
  print("Not a sensitivity table!")
}

defectList.sensitivityList <- function(slist, age = NA, maxAge = NA) {

  if (is.na(age)) stop("Cannot calculate loss without age!")

  output <-
    slist %>%
    mutate(logSens = log10(sensitivity),
           type = as.character(type),
           nv = calcNV(age, type, frequency),
           loss = -10 * (nv - logSens),
           minSens = log10(1 / maxContrast),
           maxLoss = -10 * (nv - minSens),
           # Calculate loss corrected for limited dynamic range in older patients
           maxAge = maxAge,
           nvMaxAge = calcNV(maxAge, type, frequency),
           dynamicRange = -10 * (nvMaxAge - minSens)
    )

  class(output) <-
    class(output) %>%
    append("sensitivityList") %>%
    append("defectList")

  output

}

##### class probabilityList #####

probabilityList <- function(x, age) UseMethod("probabilityList")

probabilityList.default <- function(x, age) {
  print("Not a sensitivity table!")
}

probabilityList.sensitivityList <- function(slist, age = NA) {

  if (is.na(age)) stop("Cannot calculate probability without age!")

  inInterval <- function(values, Type, Frequency) {

    if (Frequency > 20) {
      warning("probabilityList: frequency too high!")
      print(Type)
      return(NA)
    }

    output <- findInterval(values,
                 NVprobsSmooth %>%
                   filter(frequency == Frequency, type == Type) %>%
                   select(p010:p990))


    output
  }

  inInterval <- Vectorize(inInterval)

  output <-
    slist %>%
    mutate(logSens = log10(sensitivity),
           type = as.character(type),
           nv = calcNV(age, type, frequency),
           loss = -10 * (nv - logSens),
           interval  = inInterval(loss, type, frequency)
    )

  class(output) <-
    class(output) %>%
    append("sensitivityList") %>%
    append("probabilityList")

  output

}

##### MD #####

lowFreq <- c(1, 2, 4, 6, 8)
highFreq <- c(10, 12, 16, 20, 28)
allFreq <- c(1, 2, 4, 6, 8, 10, 12, 16, 20, 28)

calcMD <- function(x, frequencies) UseMethod("calcMD")

calcMD.default <- function(x, frequencies) {
  print(paste("calcMD: method not available for class:", class(x)))
}

calcMD.defectList <- function(dlist, frequencies = c(1, 2, 4, 6, 8, 10, 12, 16, 20, 28)) {

  output <-
    dlist %>%
    filter(frequency %in% frequencies) %>%
    group_by(type) %>%
    summarize(patid = first(basics.subject),
              eye = first(basics.eye),
              md = mean(loss)) %>%
    ungroup()

  output <- left_join(data.frame(type = c("L", "M", "S", "R")), output)

  outputTransposed <- data.frame(t(output$md))
  names(outputTransposed) <- t(output$type)
  outputTransposed <- data.frame(patid = first(output$patid),
                                 eye = first(output$eye),
                                 outputTransposed)

  outputTransposed

}

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







