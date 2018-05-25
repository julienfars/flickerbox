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
