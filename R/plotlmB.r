# plotlmB.r
# written by JuG
# December 20 2017


#' Plot 2D regression line from lmB
#' @author JuG
#' @description #' Plot 2D regression line from lmB
#' @param x
#' @param y
#' @param data
#' @param mcmcList a mcmc list (out put from lmB)
#' @param prediction if TRUE, prediction intervals are drawn
#' @details
#' @examples
#' nbPoints = 20
#' x <-seq(1,10, length = nbPoints)
#' y <- 3 * x + rnorm(nbPoints, 0,6) + 10
#' outlmB <- lmB(formula = y~x,returnCodaSamples = T)
#' plotlmB(x,y,mcmcList = outlmB, ylim=c(0,50))
#' @return
#' @export


plotlmB <- function(x, y, mcmcList, data=NULL, ...) {
  plot(y ~ x, data = df, ...)
  # add fill
  for (i in 1:500) {
    ind <- sample(size = 1, 1:dim(mcmcList[[1]])[1])
    abline(a = mcmcList[[1]][ind, 1], b = mcmcList[[1]][ind, 2], col = rgb(1, 0, 0, .1))
  }

  abline(a = median(mcmcList[[1]][, 1]), b = median(mcmcList[[1]][, 2]), lwd = 2)
  points(y ~ x, data = df, pch = 19, col = "blue")

}
