#' @export
fitModel <- function(ellipse, freq, a1 = 1, a2 = 1, alpha = 1) {

  xvalues <- c(ellipse[ellipse$frequency == freq, "X"])
  yvalues <- c(ellipse[ellipse$frequency == freq, "Y"])

  k <- yvalues / xvalues
  xtimesy <- sqrt(xvalues ^ 2 + yvalues ^ 2)

  model <- minpack.lm::nlsLM(
    xtimesy ~ ellipseFunction(k, a1, a2, alpha),
    start = list(
      a1 = a1,
      a2 = a2,
      alpha = alpha
    ),
    control = list(maxiter = 800, ftol = 0.001836)
  )

  yvaluesPredict <- predict(model)
  xvaluesPredict <- ifelse(k == Inf, 0, yvaluesPredict / k)

  xneu <- c(xvaluesPredict, -xvaluesPredict)
  yneu <- c(yvaluesPredict, -yvaluesPredict)
  tabe <- data.frame(xneu, yneu)

  hab <- data.frame(summary(model)$parameters)
  b1 <- hab[1, 1]
  b2 <- hab[2, 1]
  beta <- hab[3, 1]
  ang <- seq(0, 360, 1)
  k <- sin(ang * pi / 180) / cos(ang * pi / 180)

  output <- list()

  output$values <- data.frame(Names = c("a1", "a2", "alpha"),
                              Values = hab)

  output$rsquared <- sum(residuals(model) ^ 2)

  output$k <- k

  output$results <- data.frame(
    Rsquared = sum(residuals(model) ^ 2),
    a1Estimate = hab[1, 1],
    a1StdError = hab[1, 2],
    a1tvalue = hab[1, 3],
    a1Pr = hab[1, 4],
    a2Estimate = hab[2, 1],
    a2StdError = hab[2, 2],
    a2tvalue = hab[2, 3],
    a2Pr = hab[2, 4],
    alphaEstimate = hab[3, 1],
    alphaStdError = hab[3, 2],
    alphatvalue = hab[3, 3],
    alphaPr = hab[3, 4]
  )

  return(output)
}
