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
