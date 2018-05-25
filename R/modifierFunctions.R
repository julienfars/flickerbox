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
