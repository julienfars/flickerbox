#' @export
plotEllipse <- function(ellipseModel) {

  X <- c(ellipseModel$xvalues, -ellipseModel$xvalues)
  Y <- c(ellipseModel$yvalues, -ellipseModel$yvalues)

  output <-
    ggplot2::qplot(
      X,
      Y,
      # main = paste(freq, "Hz"),
      xlab = ellipseModel$coords[1],
      ylab = ellipseModel$coords[2],
      asp = 1
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0))

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

    xr2 <- c(xr, -rev(xr))
    yr2 <- c(yr, -rev(yr))
    output <-
      output +
      ggplot2::geom_path(ggplot2::aes(x = xr2, y = yr2), color = "red")

  print(output)

}
