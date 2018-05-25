#' @export
plotEllipse <- function(ellipse,
                        freq = 2,
                        ellipseModel = NULL) {
  ellipse <- dplyr::filter(ellipse, frequency == freq)


  X <- c(ellipse[, "X"],-ellipse[, "X"])
  Y <- c(ellipse[, "Y"],-ellipse[, "Y"])

  output <-
    ggplot2:qplot(
      X,
      Y,
      main = paste(freq, "Hz"),
      xlab = "M-cone contrast",
      ylab = "L-cone contrast",
      aps = 1
    ) +
    geom_point() +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(yintercept = 0))

  if (!is.null(ellipseModel)) {
    yr <- c()
    xr <- c()

    for (i in ellipseModel$k) {
      yr <- c(
        yr,
        ellipseFunction(
          i,
          ellipseModel$results$a1Estimate,
          ellipseModel$results$a2Estimate,
          ellipseModel$results$alphaEstimate,
          returnValue = "singleValues"
        )[2]
      )
      xr <- c(
        xr,
        ellipseFunction(
          i,
          ellipseModel$results$a1Estimate,
          ellipseModel$results$a2Estimate,
          ellipseModel$results$alphaEstimate,
          returnValue = "singleValues"
        )[1]
      )
    }

    xr2 <- c(xr, -xr)
    yr2 <- c(yr, -yr)
    output <-
      output +
      geom_lines(aes(x = xr2, y = yr2), color = "red")
  }

  print(output)

}
