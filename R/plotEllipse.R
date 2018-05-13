#' @export
plotEllipse <- function(ellipse,
                        freq = 2,
                        showFit = T) {
  ellipse <- ellipse %>% filter(frequency == freq)


  X <- c(ellipse[, "X"],-ellipse[, "X"])
  Y <- c(ellipse[, "Y"],-ellipse[, "Y"])

  if (showFit)
  {
    Efit <- fit.ellipse(X, Y)
    E <- get.ellipse(Efit, n = 360)
  }

  plot(
    X,
    Y,
    main = "2Hz",
    xlab = "M-cone contrast",
    ylab = "L-cone contrast",
    asp = 1
  )

  abline(h = 0)
  abline(v = 0)

  if (showFit) {
    lines(-E[, 1], E[, 2])
  }

}
