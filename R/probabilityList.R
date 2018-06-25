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
                             dplyr::filter(frequency == Frequency, type == Type) %>%
                             dplyr::select(p010:p990))


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
